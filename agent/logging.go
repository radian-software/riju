package main

import (
	"fmt"
	"io"
	"log"
)

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
