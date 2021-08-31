package main

// Based in part on
// https://github.com/gorilla/websocket/blob/master/examples/echo/client.go

import (
	"encoding/json"
	"fmt"
	"log"
	"net/url"
	"os"
	"os/signal"
	"path"
	"syscall"

	"github.com/alecthomas/kong"
	"github.com/gorilla/websocket"
	"github.com/pkg/errors"
	"github.com/pkg/term/termios"
	"golang.org/x/sys/unix"
)

var cli struct {
	Lang string `arg:"" help:"Name of programming language."`
	File string `arg:"" optional:"" type:"existingfile" help:"File to run."`
	Raw  bool   `short:"r" default:"false" help:"Pass ctrl-C to Riju instead of terminating the connection."`
	Host string `default:"https://riju.codes/api/v1" help:"URL of Riju API."`
}

type message struct {
	Event string `json:"event"`
}

type langConfig struct {
	message
	Config struct {
		Id   string `json:"id"`
		Name string `json:"name"`
		Repl string `json:"repl"`
	} `json:"config"`
}

type errorMessage struct {
	message
	Error string `json:"errorMessage"`
}

type terminalInput struct {
	message
	Input string `json:"input"`
}

type terminalOutput struct {
	message
	Output string `json:"output"`
}

type serviceFailed struct {
	message
	Service string `json:"service"`
	Error   string `json:"error"`
	Code    int    `json:"code"`
}

type errorExit struct {
	error
	status int
}

func run() error {
	apiUrl, err := url.Parse(cli.Host)
	if err != nil {
		return err
	}
	scheme := "wss"
	if apiUrl.Scheme == "http" {
		scheme = "ws"
	}
	socketUrl := url.URL{
		Scheme: scheme,
		Host:   apiUrl.Host,
		Path:   path.Join(apiUrl.Path, "ws"),
		RawQuery: url.Values{
			"lang": []string{cli.Lang},
		}.Encode(),
	}
	conn, _, err := websocket.DefaultDialer.Dial(socketUrl.String(), nil)
	if err != nil {
		return err
	}
	defer conn.Close()
	var origAttr unix.Termios
	if err := termios.Tcgetattr(os.Stdin.Fd(), &origAttr); err != nil {
		return err
	}
	rawAttr := origAttr
	termios.Cfmakeraw(&rawAttr)
	if !cli.Raw {
		// Do not pass ctrl-C over pty, instead invoke our
		// signal handler and abort.
		rawAttr.Lflag |= syscall.ISIG
	}
	if err := termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &rawAttr); err != nil {
		return err
	}
	defer termios.Tcsetattr(os.Stdin.Fd(), termios.TCSAFLUSH, &origAttr)
	sigint := make(chan os.Signal, 1)
	sigterm := make(chan os.Signal, 1)
	signal.Notify(sigint, syscall.SIGINT)
	signal.Notify(sigterm, syscall.SIGTERM)
	done1 := make(chan error)
	go func() {
		defer close(done1)
		for {
			_, rawMsg, err := conn.ReadMessage()
			if err != nil {
				done1 <- errors.Wrap(err, "failed to read websocket message")
				return
			}
			var genericMsg message
			if err := json.Unmarshal(rawMsg, &genericMsg); err != nil {
				done1 <- errors.Wrap(err, "failed to parse websocket message")
				return
			}
			switch genericMsg.Event {
			case "error":
				var msg errorMessage
				if err := json.Unmarshal(rawMsg, &msg); err != nil {
					done1 <- errors.Wrap(err, "failed to parse websocket message")
					return
				}
				done1 <- errors.New(msg.Error)
			case "langConfig":
				var msg langConfig
				if err := json.Unmarshal(rawMsg, &msg); err != nil {
					done1 <- errors.Wrap(err, "failed to parse websocket message")
					return
				}
				if msg.Config.Repl == "" {
					done1 <- fmt.Errorf("%s has no repl, you must provide a file to run", msg.Config.Name)
					return
				}
			case "terminalOutput":
				var msg terminalOutput
				if err := json.Unmarshal(rawMsg, &msg); err != nil {
					done1 <- errors.Wrap(err, "failed to parse websocket message")
					return
				}
				fmt.Print(msg.Output)
			case "serviceFailed":
				var msg serviceFailed
				if err := json.Unmarshal(rawMsg, &msg); err != nil {
					done1 <- errors.Wrap(err, "failed to parse websocket message")
					return
				}
				done1 <- errorExit{nil, msg.Code}
				return
			}
		}
	}()
	input := make(chan []byte)
	done2 := make(chan error)
	go func() {
		defer close(done2)
		for {
			buf := make([]byte, 1024)
			n, err := os.Stdin.Read(buf)
			if err != nil {
				done2 <- errors.Wrap(err, "failed to read from stdin")
				return
			}
			input <- buf[:n]
		}
	}()
	for {
		select {
		case err := <-done1:
			return err
		case err := <-done2:
			return err
		case <-sigint:
			return errorExit{nil, int(syscall.SIGINT) + 128}
		case <-sigterm:
			return errorExit{nil, int(syscall.SIGTERM) + 128}
		case data := <-input:
			msg := terminalInput{
				message: message{
					Event: "terminalInput",
				},
				Input: string(data),
			}
			rawMsg, err := json.Marshal(&msg)
			if err != nil {
				return errors.Wrap(err, "failed to create websocket message")
			}
			if err := conn.WriteMessage(websocket.TextMessage, rawMsg); err != nil {
				return errors.Wrap(err, "failed to send websocket message")
			}
		}
	}
	return nil
}

func main() {
	log.SetPrefix("riju: ")
	log.SetFlags(0)
	kong.Parse(&cli)
	exitStatus := 0
	if err := run(); err != nil {
		if typedErr, ok := err.(errorExit); ok {
			err = typedErr.error
			exitStatus = typedErr.status
		} else {
			exitStatus = 1
		}
		if err != nil {
			log.Println(err)
		}
	}
	os.Exit(exitStatus)
}
