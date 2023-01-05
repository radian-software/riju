package main

import (
	"encoding/json"
	"fmt"
	"net/http"
	"os"
	"os/exec"
	"time"

	"github.com/google/shlex"
	"github.com/gorilla/websocket"
)

type clientMessage struct {
	// "stdin"
	Event string `json:"event"`
	// contents of stdin
	Data []byte `json:"data,omitempty"`
}

type serverMessage struct {
	// "stdout", "stderr", "exit", "warn", "error"
	Event string `json:"event"`
	// contents of stdout/stderr
	Data []byte `json:"data,omitempty"`
	// error message
	Text string `json:"text,omitempty"`
	// exit status
	ExitStatus *int `json:"exitStatus,omitempty"`
}

var upgrader = websocket.Upgrader{}

func closeWs(ms *ManagedWebsocket) {
	ms.CloseChan <- struct{}{}
}

func send(ms *ManagedWebsocket, msg *serverMessage) {
	data, err := json.Marshal(msg)
	if err != nil {
		logErrorf("marshaling message: %w", err)
		closeWs(ms)
		return
	}
	ms.OutgoingChan <- data
}

func fatal(ms *ManagedWebsocket, err error) {
	send(ms, &serverMessage{
		Event: "fatal",
		Text:  err.Error(),
	})
}

func fatalf(ms *ManagedWebsocket, format string, arg ...interface{}) {
	fatal(ms, fmt.Errorf(format, arg...))
}

func warn(ms *ManagedWebsocket, err error) {
	send(ms, &serverMessage{
		Event: "warn",
		Text:  err.Error(),
	})
}

func warnf(ms *ManagedWebsocket, format string, arg ...interface{}) {
	warn(ms, fmt.Errorf(format, arg...))
}

func getCommandPrefix() []string {
	prefix := os.Getenv("RIJU_AGENT_COMMAND_PREFIX")
	if prefix == "" {
		logErrorf("must specify RIJU_AGENT_COMMAND_PREFIX for security reasons")
		os.Exit(1)
	}
	if prefix == "0" {
		return []string{}
	}
	list, err := shlex.Split(prefix)
	if err != nil {
		logErrorf("parsing RIJU_AGENT_COMMAND_PREFIX: %w", err)
		os.Exit(1)
	}
	return list
}

var CommandPrefix = getCommandPrefix()

// https://github.com/gorilla/websocket/blob/76ecc29eff79f0cedf70c530605e486fc32131d1/examples/command/main.go
func handler(w http.ResponseWriter, r *http.Request) {
	// Upgrade http connection to websocket
	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		logErrorf("upgrading connection: %w", err)
		return
	}
	// Set up channels to handle incoming and outgoing websocket
	// messages more conveniently, and also to handle closing the
	// websocket on error or when we ask.
	ms := &ManagedWebsocket{
		Socket: ws,

		MessageType:  websocket.TextMessage,
		PingPeriod:   5 * time.Second,
		ReadTimeout:  10 * time.Second,
		WriteTimeout: 10 * time.Second,
	}
	ms.Init()
	// Ensure that websocket will be closed eventually when we
	// exit.
	defer closeWs(ms)
	// Parse request query parameters; do this after upgrading to
	// websocket so that we can send errors back on the websocket
	// which is easier for clients to parse
	cmdline := r.URL.Query()["cmdline"]
	if len(cmdline) == 0 {
		fatalf(ms, "cmdline query parameter missing")
		return
	}
	cmdline = append(CommandPrefix, cmdline...)
	binary, err := exec.LookPath(cmdline[0])
	if err != nil {
		fatalf(ms, "searching for executable: %w", err)
		return
	}
	// Spawn subprocess
	mp, err := NewManagedProcess(binary, cmdline, nil)
	if err != nil {
		fatalf(ms, "spawning process: %w", err)
		return
	}
	// Ensure eventual process termination
	defer func() {
		mp.CloseChan <- struct{}{}
	}()
	// Handle received messages from client
	go func() {
		for data := range ms.IncomingChan {
			msg := clientMessage{}
			err := json.Unmarshal(data, &msg)
			if err != nil {
				warnf(ms, "parsing json: %w", err)
				continue
			}
			switch msg.Event {
			case "stdin":
				mp.StdinChan <- msg.Data
			default:
				logWarnf("received unknown event type %s", msg.Event)
			}
		}
	}()
	// Proxy stdout and stderr from subprocess
	go func() {
		for data := range mp.StdoutChan {
			msg, err := json.Marshal(&serverMessage{
				Event: "stdout",
				Data:  data,
			})
			if err != nil {
				warnf(ms, "wrapping stdout in json: %w", err)
				return
			}
			ms.OutgoingChan <- msg
		}
	}()
	go func() {
		for data := range mp.StderrChan {
			msg, err := json.Marshal(&serverMessage{
				Event: "stderr",
				Data:  data,
			})
			if err != nil {
				warnf(ms, "wrapping stderr in json: %w", err)
				return
			}
			ms.OutgoingChan <- msg
		}
	}()
	// Send info about process exit status
	exitChan2 := make(chan struct{}, 16)
	go func() {
		for status := range mp.ExitChan {
			exitChan2 <- struct{}{}
			code := status.ExitCode()
			send(ms, &serverMessage{
				Event:      "exit",
				ExitStatus: &code,
			})
		}
	}()
	// Wait until one of subprocess or websocket exits. The other
	// one will be cleaned up on return.
	select {
	case <-exitChan2:
	case <-ms.ClosedChan:
	}
	// Wait a bit to send any pending messages before closing the
	// connection.
	time.Sleep(1 * time.Second)
	return
}

func main() {
	port := os.Getenv("RIJU_AGENT_PORT")
	if port == "" {
		port = "869"
	}
	host := os.Getenv("RIJU_AGENT_HOST")
	if host == "" {
		host = "0.0.0.0"
	}
	fmt.Printf("Listening on http://%s:%s\n", host, port)
	mux := http.NewServeMux()
	mux.HandleFunc("/exec", handler)
	mux.HandleFunc("/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	})
	err := http.ListenAndServe(fmt.Sprintf("%s:%s", host, port), mux)
	if err != nil {
		logError(err)
		os.Exit(1)
	}
}
