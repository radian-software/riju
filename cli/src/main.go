package main

// Based in part on
// https://github.com/gorilla/websocket/blob/master/examples/echo/client.go

import (
	"encoding/json"
	"fmt"
	"log"
	"net/url"
	"path"

	"github.com/alecthomas/kong"
	"github.com/gorilla/websocket"
)

var cli struct {
	Lang string `arg help:"Name of programming language."`
	File string `arg optional type:"path" help:"File to run."`
	Host string `default:"https://riju.codes/api/v1" help:"URL of Riju API."`
}

type message struct {
	Event string `json:"event"`
}

type terminalInput struct {
	message
	Input string `json:"input"`
}

type terminalOutput struct {
	message
	Output string `json:"output"`
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
	done := make(chan struct{})
	go func() {
		defer close(done)
		for {
			_, rawMsg, err := conn.ReadMessage()
			if err != nil {
				log.Println("failed to read websocket message:", err)
				return
			}
			var genericMsg message
			if err := json.Unmarshal(rawMsg, &genericMsg); err != nil {
				log.Println("failed to parse websocket message:", err)
			}
			switch genericMsg.Event {
			case "terminalOutput":
				var msg terminalOutput
				if err := json.Unmarshal(rawMsg, &msg); err != nil {
					log.Println("failed to parse websocket message:", err)
				}
				fmt.Print(msg.Output)
			}
		}
	}()
	for {
		select {
		case <-done:
			return nil
		}
	}
	return nil
}

func main() {
	kong.Parse(&cli)
	if err := run(); err != nil {
		log.Fatalln(err)
	}
}
