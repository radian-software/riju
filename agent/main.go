package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"syscall"
	"time"

	"github.com/gorilla/websocket"
)

type clientMessage struct {
	// "stdin"
	Event string `json:"event"`
	// contents of stdin
	Data []byte `json:"data,omitempty"`
}

type serverMessage struct {
	// "start", "stdout", "stderr", "exit", "warn", "error"
	Event string `json:"event"`
	// contents of stdout/stderr
	Data []byte `json:"data,omitempty"`
	// error message
	Text string `json:"text,omitempty"`
	// exit status
	ExitStatus *int `json:"exitStatus,omitempty"`
}

const (
	pingPeriod    = 15 * time.Second
	pongWait      = 10 * time.Second
	writeDeadline = 1 * time.Second
)

var upgrader = websocket.Upgrader{}

func logWarn(err error) {
	log.Println(err.Error())
}

func logWarnf(format string, arg ...interface{}) {
	logWarn(fmt.Errorf(format, arg...))
}

func logError(err error) {
	log.Println(err.Error())
}

func logErrorf(format string, arg ...interface{}) {
	logError(fmt.Errorf(format, arg...))
}

func tryClose(obj io.Closer, objName string) {
	err := obj.Close()
	if err != nil {
		logErrorf("error closing %s: %w", objName, err)
	}
}

func closeWs(ws *websocket.Conn) {
	err := ws.WriteMessage(websocket.CloseMessage, websocket.FormatCloseMessage(websocket.CloseNormalClosure, ""))
	if err != nil {
		logErrorf("sending close message: %w", err)
	}
	tryClose(ws, "websocket")
}

func send(ws *websocket.Conn, msg *serverMessage) {
	data, err := json.Marshal(msg)
	if err != nil {
		logErrorf("marshaling message: %w", err)
		closeWs(ws)
		return
	}
	err = ws.WriteMessage(websocket.TextMessage, data)
	if err != nil {
		logErrorf("sending message: %w", err)
		closeWs(ws)
		return
	}
}

func fatal(ws *websocket.Conn, err error) {
	send(ws, &serverMessage{
		Event: "fatal",
		Text:  err.Error(),
	})
}

func fatalf(ws *websocket.Conn, format string, arg ...interface{}) {
	fatal(ws, fmt.Errorf(format, arg...))
}

func warn(ws *websocket.Conn, err error) {
	send(ws, &serverMessage{
		Event: "warn",
		Text:  err.Error(),
	})
}

func warnf(ws *websocket.Conn, format string, arg ...interface{}) {
	warn(ws, fmt.Errorf(format, arg...))
}

func handleClientMessages(ws *websocket.Conn, stdinChan chan<- []byte) {
	// Close channel after we exit
	defer close(stdinChan)
	// Stop processing reads some time after we stop receiving
	// timely responses to our pings.
	ws.SetReadDeadline(time.Now().Add(pongWait))
	ws.SetPongHandler(func(string) error {
		ws.SetReadDeadline(time.Now().Add(pongWait))
		return nil
	})
	// Read data and dispatch appropriately. Return on timeout or
	// error. Caller is responsible for cleanup.
	for {
		msgtype, data, err := ws.ReadMessage()
		if err != nil {
			fatalf(ws, "reading message: %w", err)
			return
		}
		if msgtype != websocket.TextMessage {
			fatalf(ws, "received non-text message type %d", msgtype)
			return
		}
		msg := clientMessage{}
		err = json.Unmarshal(data, &msg)
		if err != nil {
			fatalf(ws, "parsing json: %w", err)
			return
		}
		switch msg.Event {
		case "stdin":
			stdinChan <- msg.Data
		default:
			logWarnf("received unknown event type %s", msg.Event)
		}
	}
}

// https://github.com/gorilla/websocket/blob/76ecc29eff79f0cedf70c530605e486fc32131d1/examples/command/main.go
func handler(w http.ResponseWriter, r *http.Request) {
	// Upgrade http connection to websocket
	ws, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		logErrorf("upgrading connection: %w", err)
		return
	}
	// Close websocket on error or when we exit
	defer closeWs(ws)
	// Parse request query parameters; do this after upgrading to
	// websocket so that we can send errors back on the websocket
	// which is easier for clients to parse
	cmdline := r.URL.Query()["cmdline"]
	if len(cmdline) == 0 {
		fatalf(ws, "cmdline query parameter missing")
		return
	}
	// Create pipes for communicating with subprocess
	stdinRead, stdinWrite, err := os.Pipe()
	if err != nil {
		fatalf(ws, "creating stdin pipe: %w", err)
		return
	}
	defer tryClose(stdinRead, "read end of stdin pipe")
	defer tryClose(stdinWrite, "write end of stdin pipe")
	stdoutRead, stdoutWrite, err := os.Pipe()
	if err != nil {
		fatalf(ws, "creating stdout pipe: %w", err)
		return
	}
	defer tryClose(stdoutRead, "read end of stdout pipe")
	defer tryClose(stdoutWrite, "write end of stdout pipe")
	stderrRead, stderrWrite, err := os.Pipe()
	if err != nil {
		fatalf(ws, "creating stderr pipe: %w", err)
		return
	}
	defer tryClose(stderrRead, "read end of stderr pipe")
	defer tryClose(stderrWrite, "write end of stderr pipe")
	// Spawn subprocess
	proc, err := os.StartProcess(cmdline[0], cmdline, &os.ProcAttr{
		Files: []*os.File{stdinRead, stdoutWrite, stderrWrite},
	})
	if err != nil {
		fatalf(ws, "spawning process: %w", err)
		return
	}
	// Setup a way for other goroutines to report a fatal error,
	// use increased capacity to avoid blockage with large number
	// of write callsites
	doneChan := make(chan struct{}, 10)
	// Setup channels and variables to monitor process state
	waitChan := make(chan struct{}, 1)
	state := (*os.ProcessState)(nil)
	// Monitor the process to see when it exits
	go func() {
		s, err := proc.Wait()
		if err != nil {
			fatalf(ws, "waiting for process to exit: %w", err)
		} else {
			state = s
		}
		waitChan <- struct{}{}
		doneChan <- struct{}{}
	}()
	// Arrange to send information about the process exit status
	// if we have obtained it by the time we return
	defer func() {
		if state != nil {
			status := state.ExitCode()
			send(ws, &serverMessage{
				Event:      "exit",
				ExitStatus: &status,
			})
		}
	}()
	// Arrange for subprocess to be killed when we exit
	defer func() {
		// See if process has already exited or is about to
		select {
		case <-waitChan:
			return
		case <-time.NewTimer(500 * time.Millisecond).C:
			//
		}
		// Try killing the process by closing stdin
		tryClose(stdinWrite, "stdin to child")
		select {
		case <-waitChan:
			return
		case <-time.NewTimer(500 * time.Millisecond).C:
			//
		}
		// Try killing the process with SIGTERM, SIGINT, then
		// finally SIGKILL
		for _, sig := range []os.Signal{syscall.SIGTERM, syscall.SIGINT, syscall.SIGKILL} {
			err = proc.Signal(sig)
			if err != nil {
				logWarnf("sending %s to child: %w", sig.String(), err)
			}
			select {
			case <-waitChan:
				return
			case <-time.NewTimer(500 * time.Millisecond).C:
				//
			}
		}
		// We are unable to kill the process
		fatalf(ws, "unable to kill child")
	}()
	// Close our copies of pipe ends passed to subprocess
	err = stdinRead.Close()
	if err != nil {
		fatalf(ws, "closing read end of stdin pipe from parent")
		return
	}
	err = stdoutWrite.Close()
	if err != nil {
		fatalf(ws, "closing write end of stdout pipe from parent")
		return
	}
	err = stderrWrite.Close()
	if err != nil {
		fatalf(ws, "closing write end of stderr pipe from parent")
		return
	}
	// Handle received messages from client
	stdinChan := make(chan []byte)
	go func() {
		handleClientMessages(ws, stdinChan)
		doneChan <- struct{}{}
	}()
	go func() {
		for data := range stdinChan {
			_, err := stdinWrite.Write(data)
			if err != nil {
				warnf(ws, "writing to stdin: %w", err)
				return
			}
		}
	}()
	// Send regular pings to ensure we get regular pongs to
	// satisfy the read deadline on handleClientMessages
	pingDoneChan := make(chan struct{}, 1)
	defer func() {
		pingDoneChan <- struct{}{}
	}()
	go func() {
		ticker := time.NewTicker(pingPeriod)
		defer ticker.Stop()
		for {
			select {
			case <-ticker.C:
				err := ws.WriteControl(websocket.PingMessage, []byte{}, time.Now().Add(writeDeadline))
				if err != nil {
					logErrorf("sending ping: %w", err)
					doneChan <- struct{}{}
					return
				}
			case <-pingDoneChan:
				return
			}
		}
	}()
	// Proxy stdout and stderr back to client
	go func() {
		for {
			buf := make([]byte, 1024)
			nr, err := stdoutRead.Read(buf)
			if err != nil {
				warnf(ws, "reading from stdout: %w", err)
				return
			}
			if nr == 0 {
				continue
			}
			data, err := json.Marshal(&serverMessage{
				Event: "stdout",
				Data:  buf[:nr],
			})
			if err != nil {
				fatalf(ws, "wrapping stdout in json: %w", err)
				doneChan <- struct{}{}
				return
			}
			ws.SetWriteDeadline(time.Now().Add(writeDeadline))
			err = ws.WriteMessage(websocket.TextMessage, data)
			if err != nil {
				fatalf(ws, "sending message: %w", err)
				doneChan <- struct{}{}
				return
			}
		}
	}()
	// Wait until either process is exited or a websocket
	// operation fails
	<-doneChan
	// Process and websocket will be cleaned up after return
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
	err := http.ListenAndServe(fmt.Sprintf("%s:%s", host, port), http.HandlerFunc(handler))
	if err != nil {
		logError(err)
		os.Exit(1)
	}
}
