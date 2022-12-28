package main

import (
	"time"

	"github.com/gorilla/websocket"
)

type ManagedWebsocket struct {
	Socket *websocket.Conn

	MessageType  int
	PingPeriod   time.Duration
	ReadTimeout  time.Duration
	WriteTimeout time.Duration

	IncomingChan chan []byte
	OutgoingChan chan []byte
	CloseChan    chan struct{}
	ClosedChan   chan struct{}
}

func (m *ManagedWebsocket) handleIncoming() {
	pongChan := make(chan struct{}, 16)
	m.Socket.SetPongHandler(func(string) error {
		pongChan <- struct{}{}
		return nil
	})
	msgChan := make(chan []byte, 16)
	go func() {
		defer close(msgChan)
		for {
			msgtype, data, err := m.Socket.ReadMessage()
			if err != nil {
				m.Socket.Close()
				return
			}
			if msgtype != m.MessageType {
				logWarnf("ignoring message of unexpected type %d", msgtype)
				continue
			}
			msgChan <- data

		}
	}()
	for {
		m.Socket.SetReadDeadline(time.Now().Add(m.ReadTimeout))
		var msgtype int
		var msgdata []byte
		select {
		case <-pongChan:
			msgtype = websocket.PongMessage
		case data := <-msgChan:
			msgtype = m.MessageType
			msgdata = data
		}
		if msgtype != m.MessageType {
			continue
		}
		m.OutgoingChan <- msgdata
	}
}

func (m *ManagedWebsocket) handleOutgoing() {
	pingTicker := time.NewTicker(m.PingPeriod)
	defer pingTicker.Stop()
	defer func() {
		m.ClosedChan <- struct{}{}
	}()
	for {
		var msgtype int
		var msgdata []byte
		select {
		case <-pingTicker.C:
			msgtype = websocket.PingMessage
			msgdata = []byte{}
		case data := <-m.OutgoingChan:
			msgtype = m.MessageType
			msgdata = data
		case <-m.CloseChan:
			msgtype = websocket.CloseMessage
			msgdata = websocket.FormatCloseMessage(websocket.CloseNormalClosure, "")
		}
		wd := time.Now().Add(m.WriteTimeout)
		m.Socket.SetWriteDeadline(wd)
		err := m.Socket.WriteMessage(msgtype, msgdata)
		if err != nil {
			m.Socket.Close()
			return
		}
		if msgtype == websocket.CloseMessage {
			time.Sleep(wd.Sub(time.Now()))
			m.Socket.Close()
			return
		}
	}
}

func (m *ManagedWebsocket) Init() {
	m.IncomingChan = make(chan []byte, 16)
	m.OutgoingChan = make(chan []byte, 16)
	m.CloseChan = make(chan struct{}, 16)
	m.ClosedChan = make(chan struct{}, 16)

	go m.handleIncoming()
	go m.handleOutgoing()
}
