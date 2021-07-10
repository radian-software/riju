package main

import (
	"bytes"
	"context"
	"crypto/sha1"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"os/exec"
	"regexp"
	"sort"
	"strings"
	"sync"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	awsConfig "github.com/aws/aws-sdk-go-v2/config"
	s3manager "github.com/aws/aws-sdk-go-v2/feature/s3/manager"
	"github.com/aws/aws-sdk-go-v2/service/ecr"
	"github.com/aws/aws-sdk-go-v2/service/s3"
	"github.com/aws/aws-sdk-go-v2/service/sts"
	"github.com/caarlos0/env/v6"
	uuidlib "github.com/google/uuid"
)

const bluePort = 6229
const greenPort = 6230

const blueName = "riju-app-blue"
const greenName = "riju-app-green"

type deploymentConfig struct {
	AppImageTag string `json:"appImageTag"`
	LangImageTags map[string]string `json:"langImageTags"`
}

type supervisorConfig struct {
	AccessToken string `env:"SUPERVISOR_ACCESS_TOKEN,notEmpty"`
	S3Bucket string `env:"S3_BUCKET,notEmpty"`
}

type reloadJob struct {
	status string
	active bool
	failed bool
}

type supervisor struct {
	config supervisorConfig

	blueProxyHandler http.Handler
	greenProxyHandler http.Handler
	isGreen bool  // blue-green deployment
	deployConfigHash string

	awsAccountNumber string
	awsRegion string
	s3 *s3.Client
	ecr *ecr.Client

	reloadLock sync.Mutex
	reloadInProgress bool
	reloadNeeded bool
	reloadUUID string
	reloadNextUUID string
	reloadJobs map[string]*reloadJob
}

func (sv *supervisor) status(status string) {
	sv.reloadLock.Lock()
	sv.reloadJobs[sv.reloadUUID].status = status
	sv.reloadLock.Unlock()
	log.Println("active: " + status)
}

func (sv *supervisor) scheduleReload() string {
	uuid := ""
	sv.reloadLock.Lock()
	if !sv.reloadInProgress {
		sv.reloadInProgress = true
		sv.reloadUUID = uuidlib.New().String()
		uuid = sv.reloadUUID
		go sv.reloadWithScheduling()
	} else {
		if sv.reloadInProgress {
			uuid = sv.reloadNextUUID
		} else {
			sv.reloadNextUUID = uuidlib.New().String()
			uuid = sv.reloadNextUUID
		}
		sv.reloadNeeded = true
	}
	sv.reloadLock.Unlock()
	return uuid
}

func (sv *supervisor) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if strings.HasPrefix(r.URL.Path, "/api/supervisor") {
		authHeader := r.Header.Get("Authorization")
		if authHeader == "" {
			http.Error(w, "401 Authorization header missing", http.StatusUnauthorized)
			return
		}
		if !strings.HasPrefix(authHeader, "Bearer ") {
			http.Error(w, "401 malformed Authorization header", http.StatusUnauthorized)
			return
		}
		if authHeader != "Bearer " + sv.config.AccessToken {
			http.Error(w, "401 wrong access token", http.StatusUnauthorized)
			return
		}
		if r.URL.Path == "/api/supervisor/v1/reload" {
			if r.Method != http.MethodPost {
				http.Error(w, "405 method not allowed", http.StatusMethodNotAllowed)
				return
			}
			uuid := sv.scheduleReload()
			fmt.Fprintln(w, uuid)
			return
		}
		if r.URL.Path == "/api/supervisor/v1/reload/status" {
			if r.Method != http.MethodGet {
				http.Error(w, "405 method not allowed", http.StatusMethodNotAllowed)
				return
			}
			uuid := r.URL.Query().Get("uuid")
			if uuid == "" {
				http.Error(
					w,
					"400 missing uuid query parameter",
					http.StatusBadRequest,
				)
				return
			}
			sv.reloadLock.Lock()
			job := sv.reloadJobs[uuid]
			if job == nil {
				if uuid == sv.reloadUUID || uuid == sv.reloadNextUUID {
					fmt.Fprintln(w, "queued")
				} else {
					http.Error(w, "404 no such job", http.StatusNotFound)
				}
			} else if job.active {
				fmt.Fprintln(w, "active: " + job.status)
			} else if job.failed {
				fmt.Fprintln(w, "failed: " + job.status)
			} else {
				fmt.Fprintln(w, "succeeded: " + job.status)
			}
			sv.reloadLock.Unlock()
			return
		}
		http.NotFound(w, r)
		return
	}
	if sv.isGreen {
		sv.greenProxyHandler.ServeHTTP(w, r)
	} else {
		sv.blueProxyHandler.ServeHTTP(w, r)
	}
	return
}

func (sv *supervisor) reloadWithScheduling() {
	sv.reloadLock.Lock()
	sv.reloadJobs[sv.reloadUUID] = &reloadJob{
		status: "initializing",
		active: true,
		failed: false,
	}
	sv.reloadLock.Unlock()
	err := sv.reload()
	sv.reloadLock.Lock()
	sv.reloadJobs[sv.reloadUUID].active = false
	if err != nil {
		log.Println("failed: " + err.Error())
		sv.reloadJobs[sv.reloadUUID].failed = true
		sv.reloadJobs[sv.reloadUUID].status = err.Error()
	} else {
		log.Println("succeeded")
	}
	sv.reloadInProgress = false
	sv.reloadUUID = ""
	if sv.reloadNeeded {
		sv.reloadNeeded = false
		sv.reloadInProgress = true
		sv.reloadUUID = sv.reloadNextUUID
		sv.reloadNextUUID = ""
		go sv.reloadWithScheduling()
	} else {
		go func() {
			// Arguably slightly incorrect but it's fine
			// if we reload slightly more than once per 5
			// minutes.
			time.Sleep(5 * time.Minute)
			sv.scheduleReload()
		}()
	}
	sv.reloadLock.Unlock()
}

var rijuImageRegexp = regexp.MustCompile(`(?:^|/)riju:([^<>]+)$`)

func (sv *supervisor) reload() error {
	sv.status("getting access token from ECR")
	ecrResp, err := sv.ecr.GetAuthorizationToken(
		context.Background(),
		&ecr.GetAuthorizationTokenInput{},
	)
	if err != nil {
		return err
	}
	if len(ecrResp.AuthorizationData) != 1 {
		return fmt.Errorf(
			"got unexpected number (%d) of authorization tokens",
			len(ecrResp.AuthorizationData),
		)
	}
	authInfo, err := base64.StdEncoding.DecodeString(*ecrResp.AuthorizationData[0].AuthorizationToken)
	if err != nil {
		return err
	}
	authInfoParts := strings.Split(string(authInfo), ":")
	if len(authInfoParts) != 2 {
		return errors.New("got malformed auth info from ECR")
	}
	dockerUsername := authInfoParts[0]
	dockerPassword := authInfoParts[1]
	sv.status("authenticating Docker client to ECR")
	dockerLogin := exec.Command(
		"docker", "login",
		"--username", dockerUsername,
		"--password-stdin",
		fmt.Sprintf(
			"%s.dkr.ecr.%s.amazonaws.com",
			sv.awsAccountNumber, sv.awsRegion,
		),
	)
	dockerLogin.Stdin = bytes.NewReader([]byte(dockerPassword))
	dockerLogin.Stdout = os.Stdout
	dockerLogin.Stderr = os.Stderr
	if err := dockerLogin.Run(); err != nil {
		return err
	}
	sv.status("downloading deployment config from S3")
	dl := s3manager.NewDownloader(sv.s3)
	buf := s3manager.NewWriteAtBuffer([]byte{})
	if _, err := dl.Download(context.Background(), buf, &s3.GetObjectInput{
		Bucket: &sv.config.S3Bucket,
		Key: aws.String("config.json"),
	}); err != nil {
		return err
	}
	deployCfg := deploymentConfig{}
	if err := json.Unmarshal(buf.Bytes(), &deployCfg); err != nil {
		return err
	}
	sv.status("listing locally available images")
	dockerImageLs := exec.Command(
		"docker", "image", "ls", "--format",
		"{{ .Repository }}:{{ .Tag }}",
	)
	dockerImageLs.Stderr = os.Stderr
	out, err := dockerImageLs.Output()
	if err != nil {
		return err
	}
	existingTags := map[string]bool{}
	for _, line := range strings.Split(string(out), "\n") {
		if match := rijuImageRegexp.FindStringSubmatch(line); match != nil {
			tag := match[1]
			existingTags[tag] = true
		}
	}
	neededTags := []string{}
	for _, tag := range deployCfg.LangImageTags {
		neededTags = append(neededTags, tag)
	}
	neededTags = append(neededTags, deployCfg.AppImageTag)
	sort.Strings(neededTags)
	for _, tag := range neededTags {
		if !existingTags[tag] {
			sv.status("pulling image for " + tag)
			fullImage := fmt.Sprintf(
				"%s.dkr.ecr.%s.amazonaws.com/riju:%s",
				sv.awsAccountNumber,
				sv.awsRegion,
				tag,
			)
			dockerPull := exec.Command("docker", "pull", fullImage)
			dockerPull.Stdout = os.Stdout
			dockerPull.Stderr = os.Stderr
			if err := dockerPull.Run(); err != nil {
				return err
			}
			dockerTag := exec.Command(
				"docker", "tag", fullImage,
				fmt.Sprintf("riju:%s", tag),
			)
			dockerTag.Stdout = os.Stdout
			dockerTag.Stderr = os.Stderr
			if err := dockerTag.Run(); err != nil {
				return err
			}
		}
	}
	deployCfgStr, err := json.Marshal(&deployCfg)
	if err != nil {
		return err
	}
	h := sha1.New()
	h.Write([]byte(deployCfgStr))
	deployCfgHash := fmt.Sprintf("%x", h.Sum(nil))
	if deployCfgHash == sv.deployConfigHash {
		sv.status(fmt.Sprintf("config hash remains at %s", deployCfgHash))
		return nil
	} else {
		sv.status(fmt.Sprintf(
			"config hash updated %s => %s",
			sv.deployConfigHash, deployCfgHash,
		))
	}
	var port int
	var name string
	var oldName string
	if sv.isGreen {
		port = bluePort
		name = blueName
		oldName = greenName
	} else {
		port = greenPort
		name = greenName
		oldName = blueName
	}
	sv.status("starting container " + name)
	dockerRun := exec.Command(
		"docker", "run", "-d",
		"-v", "/var/run/riju:/var/run/riju",
		"-v", "/var/run/docker.sock:/var/run/docker.sock",
		"-p", fmt.Sprintf("127.0.0.1:%d:6119", port),
		"-e", "RIJU_DEPLOY_CONFIG",
		"-e", "ANALYTICS=1",
		"--label", fmt.Sprintf("riju.deploy-config-hash=%s", deployCfgHash),
		"--name", name,
		fmt.Sprintf("riju:%s", deployCfg.AppImageTag),
	)
	dockerRun.Stdout = os.Stdout
	dockerRun.Stderr = os.Stderr
	dockerRun.Env = append(os.Environ(), fmt.Sprintf("RIJU_DEPLOY_CONFIG=%s", deployCfgStr))
	if err := dockerRun.Run(); err != nil {
		return err
	}
	sv.status("waiting for container to start up")
	time.Sleep(5 * time.Second)
	sv.status("checking that container is healthy")
	resp, err := http.Get(fmt.Sprintf("http://localhost:%d", port))
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	if !strings.Contains(string(body), "python") {
		return errors.New("container did not appear to be healthy")
	}
	sv.isGreen = !sv.isGreen
	sv.status("stopping old container")
	dockerRm := exec.Command("docker", "rm", "-f", oldName)
	dockerRm.Stdout = dockerRm.Stdout
	dockerRm.Stderr = dockerRm.Stderr
	if err := dockerRm.Run(); err != nil {
		return err
	}
	sv.status("saving updated config hash")
	sv.deployConfigHash = deployCfgHash
	sv.status("reload complete")
	return nil
}

var rijuContainerRegexp = regexp.MustCompile(`^([^:]+):(.+)$`)

func main() {
	supervisorCfg := supervisorConfig{}
	if err := env.Parse(&supervisorCfg); err != nil {
		log.Fatalln(err)
	}

	rijuInitVolume := exec.Command("riju-init-volume")
	rijuInitVolume.Stdout = rijuInitVolume.Stdout
	rijuInitVolume.Stderr = rijuInitVolume.Stderr
	if err := rijuInitVolume.Run(); err != nil {
		log.Fatalln(err)
	}

	blueUrl, err := url.Parse(fmt.Sprintf("http://localhost:%d", bluePort))
	if err != nil {
		log.Fatalln(err)
	}
	greenUrl, err := url.Parse(fmt.Sprintf("http://localhost:%d", greenPort))
	if err != nil {
		log.Fatalln(err)
	}

	awsCfg, err := awsConfig.LoadDefaultConfig(context.Background())
	if err != nil {
		log.Fatalln(err)
	}

	stsClient := sts.NewFromConfig(awsCfg)
	ident, err := stsClient.GetCallerIdentity(context.Background(), &sts.GetCallerIdentityInput{})
	if err != nil {
		log.Fatalln(err)
	}

	dockerContainerLs := exec.Command(
		"docker", "container", "ls", "-a",
		"--format", "{{ .Names }}:{{ .CreatedAt }}",
	)
	dockerContainerLs.Stderr = os.Stderr
	out, err := dockerContainerLs.Output()
	if err != nil {
		log.Fatalln(err)
	}

	var blueRunningSince *time.Time
	var greenRunningSince *time.Time
	for _, line := range strings.Split(string(out), "\n") {
		if match := rijuContainerRegexp.FindStringSubmatch(line); match != nil {
			name := match[1]
			created, err := time.Parse(
				"2006-01-02 15:04:05 -0700 MST",
				match[2],
			)
			if err != nil {
				continue
			}
			if name == blueName {
				blueRunningSince = &created
				continue
			}
			if name == greenName {
				greenRunningSince = &created
				continue
			}
		}
	}

	var isGreen bool
	var isRunning bool
	if blueRunningSince == nil && greenRunningSince == nil {
		log.Println("did not detect any existing containers")
		isGreen = false
		isRunning = true
	} else if blueRunningSince != nil && greenRunningSince == nil {
		log.Println("detected existing blue container")
		isGreen = false
		isRunning = true
	} else if greenRunningSince != nil && blueRunningSince == nil {
		log.Println("detected existing green container")
		isGreen = true
		isRunning = true
	} else {
		log.Println("detected existing blue and green containers")
		isGreen = greenRunningSince.Before(*blueRunningSince)
		var color string
		var name string
		if isGreen {
			color = "blue"
			name = blueName
		} else {
			color = "green"
			name = greenName
		}
		log.Printf("stopping %s container as it is newer\n", color)
		dockerRm := exec.Command("docker", "rm", "-f", name)
		dockerRm.Stdout = os.Stdout
		dockerRm.Stderr = os.Stderr
		if err := dockerRm.Run(); err != nil {
			log.Fatalln(err)
		}
		isRunning = true
	}

	deployCfgHash := "none"

	if isRunning {
		var name string
		if isGreen {
			name = greenName
		} else {
			name = blueName
		}
		dockerInspect := exec.Command(
			"docker", "inspect", name,
			`{{ index .Config.Labels "riju.deploy-config-hash" }}`,
		)
		dockerInspect.Stderr = os.Stderr
		out, err := dockerInspect.Output()
		if err != nil {
			log.Fatalln(err)
		}
		if hash := strings.TrimSpace(string(out)); hash != "" {
			deployCfgHash = hash
		} else {
			deployCfgHash = "unknown"
		}
	}

	sv := &supervisor{
		config: supervisorCfg,
		blueProxyHandler: httputil.NewSingleHostReverseProxy(blueUrl),
		greenProxyHandler: httputil.NewSingleHostReverseProxy(greenUrl),
		isGreen: isGreen,
		deployConfigHash: deployCfgHash,
		s3: s3.NewFromConfig(awsCfg),
		ecr: ecr.NewFromConfig(awsCfg),
		awsRegion: awsCfg.Region,
		awsAccountNumber: *ident.Account,
		reloadJobs: map[string]*reloadJob{},
	}
	go sv.scheduleReload()
	log.Println("listening on http://0.0.0.0:80")
	log.Fatalln(http.ListenAndServe("0.0.0.0:80", sv))
}
