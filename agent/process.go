package main

import (
	"fmt"
	"os"
	"syscall"
	"time"
)

type managedProcess struct {
	proc *os.Process

	stdinRead   *os.File
	stdinWrite  *os.File
	stdoutRead  *os.File
	stdoutWrite *os.File
	stderrRead  *os.File
	stderrWrite *os.File

	internalExitChan chan struct{}

	StdinChan  chan []byte
	StdoutChan chan []byte
	StderrChan chan []byte
	ExitChan   chan *os.ProcessState
	CloseChan  chan struct{}
}

func NewManagedProcess(name string, argv []string, attr *os.ProcAttr) (*managedProcess, error) {
	mp := &managedProcess{
		StdinChan:  make(chan []byte, 16),
		StdoutChan: make(chan []byte, 16),
		StderrChan: make(chan []byte, 16),
		CloseChan:  make(chan struct{}, 16),
	}
	done := false
	go mp.handleClose()
	defer func() {
		if !done {
			mp.CloseChan <- struct{}{}
		}
	}()
	var err error
	mp.stdinRead, mp.stdinWrite, err = os.Pipe()
	if err != nil {
		return mp, fmt.Errorf("creating stdin pipe: %w", err)
	}
	mp.stdoutRead, mp.stdoutWrite, err = os.Pipe()
	if err != nil {
		return mp, fmt.Errorf("creating stdout pipe: %w", err)
	}
	mp.stderrRead, mp.stderrWrite, err = os.Pipe()
	if err != nil {
		return mp, fmt.Errorf("creating stderr pipe: %w", err)
	}
	newAttr := &os.ProcAttr{}
	if attr != nil {
		*newAttr = *attr
	}
	if len(newAttr.Files) < 3 {
		newAttr.Files = append(newAttr.Files, make([]*os.File, 3-len(newAttr.Files))...)
		newAttr.Files[0] = mp.stdinRead
		newAttr.Files[1] = mp.stdoutWrite
		newAttr.Files[2] = mp.stderrWrite
	}
	mp.proc, err = os.StartProcess(name, argv, newAttr)
	if err != nil {
		return mp, fmt.Errorf("spawning process: %w", err)
	}
	go mp.handleWait()
	go mp.handleInput(mp.StdinChan, mp.stdinWrite, "stdin")
	go mp.handleOutput(mp.StdoutChan, mp.stdoutRead, "stdout")
	go mp.handleOutput(mp.StderrChan, mp.stderrRead, "stderr")
	done = true
	return mp, nil
}

func (mp *managedProcess) handleInput(ch chan []byte, f *os.File, name string) {
	for data := range ch {
		nw, err := f.Write(data)
		if err != nil {
			logWarnf("writing to %s: got error after %d of %d byte(s): %w", name, nw, len(data), err)
			return
		}
	}
}

func (mp *managedProcess) handleOutput(ch chan []byte, f *os.File, name string) {
	for {
		buf := make([]byte, 1024)
		nr, err := f.Read(buf)
		if err != nil {
			logWarnf("reading from %s: got error after %d byte(s): %w", name, nr, err)
			return
		}
		if nr == 0 {
			continue
		}
		ch <- buf[:nr]
	}
}

func (mp *managedProcess) handleWait() {
	s, err := mp.proc.Wait()
	if err != nil {
		logErrorf("waiting on process: %w", err)
	}
	mp.internalExitChan <- struct{}{}
	mp.ExitChan <- s
}

func (mp *managedProcess) killProc() {
	// See if process has already exited or is about to
	select {
	case <-mp.internalExitChan:
		return
	case <-time.NewTimer(500 * time.Millisecond).C:
		//
	}
	// Try killing the process by closing stdin
	mp.stdinWrite.Close()
	select {
	case <-mp.internalExitChan:
		return
	case <-time.NewTimer(500 * time.Millisecond).C:
		//
	}
	// Try killing the process with SIGTERM, SIGINT, then
	// finally SIGKILL
	for _, sig := range []os.Signal{syscall.SIGTERM, syscall.SIGINT, syscall.SIGKILL} {
		err := mp.proc.Signal(sig)
		if err != nil {
			logErrorf("sending %s to child: %w", sig.String(), err)
		}
		select {
		case <-mp.internalExitChan:
			return
		case <-time.NewTimer(500 * time.Millisecond).C:
			//
		}
	}
	// We are unable to kill the process
	logErrorf("unable to kill child process (pid %d)", mp.proc.Pid)
}

func (mp *managedProcess) handleClose() {
	<-mp.CloseChan
	for _, p := range []*os.File{
		mp.stdinRead, mp.stdinWrite,
		mp.stdoutRead, mp.stdoutWrite,
		mp.stderrRead, mp.stderrWrite,
	} {
		if p != nil {
			p.Close()
		}
	}
	if mp.proc != nil {
		//
	}
}
