package main

import (
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"
	"sync"
)

type supervisor struct {
	proxyHandler http.Handler

	reloadLock sync.Mutex
	reloadInProgress bool
	reloadNeeded bool
}

func (sv *supervisor) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if strings.HasPrefix(r.URL.Path, "/api/supervisor") {
		if r.URL.Path == "/api/supervisor/v1/reload" {
			resp := ""
			sv.reloadLock.Lock()
			if !sv.reloadInProgress {
				sv.reloadInProgress = true
				go sv.reload()
				resp = "Triggered reload."
			} else {
				sv.reloadNeeded = true
				resp = "Reload already in progress, scheduling another one."
			}
			sv.reloadLock.Unlock()
			fmt.Fprintln(w, resp)
			return
		}
		http.NotFound(w, r)
		return
	}
	sv.proxyHandler.ServeHTTP(w, r)
	return
}

func (sv *supervisor) reloadWithScheduling() {
	err := sv.reload()
	if err != nil {
		log.Printf("failed to reload: %s\n", err.Error())
	} else {
		log.Println("successfully reloaded")
	}
	sv.reloadLock.Lock()
	sv.reloadInProgress = false
	if sv.reloadNeeded {
		sv.reloadInProgress = true
		go sv.reloadWithScheduling()
	}
	sv.reloadLock.Unlock()
}

func (sv *supervisor) reload() error {
	fmt.Println("starting reload")
	return nil
}

func main() {
	url, err := url.Parse("http://localhost:6119")
	if err != nil {
		log.Fatal(err)
	}
	sv := &supervisor{
		proxyHandler: httputil.NewSingleHostReverseProxy(url),
	}
	log.Println("listening on http://0.0.0.0:80")
	log.Fatalln(http.ListenAndServe("0.0.0.0:80", sv))
}
