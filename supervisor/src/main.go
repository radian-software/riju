package main

import (
	"context"
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
	"strings"
	"sync"
	"time"

	"github.com/aws/aws-sdk-go-v2/aws"
	awsConfig "github.com/aws/aws-sdk-go-v2/config"
	s3manager "github.com/aws/aws-sdk-go-v2/feature/s3/manager"
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
	LangImageTags map[string]string `json:"langImageTags"`
	AppImageTag string `json:"appImageTag"`
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

	awsAccountNumber string
	awsRegion string
	s3 *s3.Client

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

func (sv *supervisor) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if strings.HasPrefix(r.URL.Path, "/api/supervisor") {
		if r.URL.Path == "/api/supervisor/v1/reload" {
			if r.Method != http.MethodPost {
				http.Error(w, "405 method not allowed", http.StatusMethodNotAllowed)
				return
			}
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
			fmt.Fprintln(w, uuid)
			return
		}
		if r.URL.Path == "/api/supervisor/v1/reload/status" {
			if r.Method != http.MethodGet {
				http.Error(w, "405 method not allowed", http.StatusMethodNotAllowed)
				return
			}
			uuids := r.URL.Query()["uuid"]
			if len(uuids) == 0 {
				http.Error(
					w,
					"400 missing uuid query parameter",
					http.StatusBadRequest,
				)
				return
			}
			if len(uuids) > 1 {
				http.Error(
					w,
					"400 more than one uuid query parameter",
					http.StatusBadRequest,
				)
				return
			}
			uuid := uuids[0]
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
	}
	sv.reloadLock.Unlock()
}

var rijuImageRegexp = regexp.MustCompile(`(?:^|/)riju:([^<>]+)$`)

func (sv *supervisor) reload() error {
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
			if err := dockerPull.Run(); err != nil {
				return err
			}
			dockerTag := exec.Command(
				"docker", "tag", fullImage,
				fmt.Sprintf("riju:%s", tag),
			)
			if err := dockerTag.Run(); err != nil {
				return err
			}
		}
	}
	deployCfgStr, err := json.Marshal(&deployCfg)
	if err != nil {
		return err
	}
	var port int
	var name string
	if sv.isGreen {
		port = bluePort
		name = blueName
	} else {
		port = greenPort
		name = greenName
	}
	sv.status("starting container " + name)
	dockerRun := exec.Command(
		"docker", "run", "-d",
		"-v", "/var/run/riju:/var/run/riju",
		"-v", "/var/run/docker.sock:/var/run/docker.sock",
		"-p", fmt.Sprintf("%s:6119", port),
		"-e", "RIJU_DEPLOY_CONFIG",
		"-e", "ANALYTICS=1",
		"--name", name,
		fmt.Sprintf("riju:%s", deployCfg.AppImageTag),
	)
	dockerRun.Env = append(os.Environ(), fmt.Sprintf("RIJU_DEPLOY_CONFIG=%s", deployCfgStr))
	if err := dockerRun.Run(); err != nil {
		return err
	}
	sv.status("waiting for container to start up")
	time.Sleep(5)
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
	sv.status("reload complete")
	return nil
}

var rijuContainerRegexp = regexp.MustCompile(`^([^:]+):(.+)$`)

func main() {
	supervisorCfg := supervisorConfig{}
	if err := env.Parse(&supervisorCfg); err != nil {
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
		"docker", "container", "ls",
		"--format", "{{ .Names }}:{{ .CreatedAt }}",
	)
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
				"2006-01-02 15:04:05 -0070 MST",
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
	if blueRunningSince == nil && greenRunningSince == nil {
		log.Println("did not detect any existing containers")
		isGreen = false
	} else if blueRunningSince != nil && greenRunningSince == nil {
		log.Println("detected existing blue container")
		isGreen = false
	} else if greenRunningSince != nil && blueRunningSince == nil {
		log.Println("detected existing green container")
		isGreen = true
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
		dockerStop := exec.Command("docker", "stop", name)
		if err := dockerStop.Run(); err != nil {
			log.Fatalln(err)
		}
	}

	sv := &supervisor{
		config: supervisorCfg,
		blueProxyHandler: httputil.NewSingleHostReverseProxy(blueUrl),
		greenProxyHandler: httputil.NewSingleHostReverseProxy(greenUrl),
		isGreen: isGreen,
		s3: s3.NewFromConfig(awsCfg),
		awsRegion: awsCfg.Region,
		awsAccountNumber: *ident.Account,
		reloadJobs: map[string]*reloadJob{},
	}
	log.Println("listening on http://0.0.0.0:80")
	log.Fatalln(http.ListenAndServe("0.0.0.0:80", sv))
}
