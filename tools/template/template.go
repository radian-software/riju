package main

import (
	"bytes"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"text/template"

	"github.com/Masterminds/sprig/v3"
	"gopkg.in/yaml.v2"
)

func findProjectRoot() (string, error) {
	curDir, err := os.Getwd()
	if err != nil {
		return "", err
	}
	prevDir := ""
	for curDir != prevDir {
		if _, err := os.Stat(filepath.Join(curDir, ".git")); err == nil {
			return curDir, nil
		} else if !os.IsNotExist(err) {
			return "", err
		}
		prevDir = curDir
		curDir = filepath.Dir(curDir)
	}
	return "", fmt.Errorf("not a git repository (or any of the parent directories): .git")
}

func mainInternal() error {
	projectRoot, err := findProjectRoot()
	if err != nil {
		return err
	}
	envText, err := os.ReadFile(filepath.Join(projectRoot, "env.yaml"))
	if err != nil {
		return err
	}
	var env interface{}
	if err := yaml.Unmarshal(envText, &env); err != nil {
		return err
	}
	if err := filepath.WalkDir(projectRoot, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.Name() == ".git" {
			return fs.SkipDir
		}
		dir := filepath.Dir(path)
		if strings.Contains(d.Name(), ".in.") {
			fmt.Fprintf(os.Stderr, "template_secrets.go: processing %s\n", path)
			input, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			tmpl, err := template.New("").Funcs(sprig.TxtFuncMap()).Option("missingkey=error").Parse(string(input))
			if err != nil {
				return err
			}
			var output bytes.Buffer
			if err := tmpl.Execute(&output, env); err != nil {
				return err
			}
			if err := os.WriteFile(
				filepath.Join(dir, strings.ReplaceAll(d.Name(), ".in.", ".out.")),
				output.Bytes(), 0644,
			); err != nil {
				return err
			}
		}
		return nil
	}); err != nil {
		return err
	}
	return nil
}

func main() {
	if err := mainInternal(); err != nil {
		fmt.Fprintf(os.Stderr, "template.go: %s\n", err.Error())
		os.Exit(1)
	}
}
