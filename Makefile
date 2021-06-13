SHELL := bash
.SHELLFLAGS := -o pipefail -euc

export PATH := bin:$(PATH)

-include .env
export

BUILD := build/$(T)/$(L)
DEB := riju-$(T)-$(L).deb
S3_DEBS := s3://$(S3_BUCKET)
S3_DEB := $(S3_DEBS)/debs/$(DEB)
S3_HASH := $(S3_DEBS)/hashes/riju-$(T)-$(L)

ifneq ($(CMD),)
BASH_CMD := bash -c '$(CMD)'
else
BASH_CMD :=
endif

# Get rid of 'Entering directory' / 'Leaving directory' messages.
MAKE_QUIETLY := MAKELEVEL= make

.PHONY: all $(MAKECMDGOALS)

 all: help

### Docker management

ifneq ($(NC),)
NO_CACHE := --no-cache
else
NO_CACHE :=
endif

## Pass NC=1 to disable the Docker cache. Base images are not pulled;
## see 'make pull-base' for that.

image: # I=<image> [L=<lang>] [NC=1] : Build a Docker image
	@: $${I}
ifeq ($(I),lang)
	@: $${L}
	node tools/build-lang-image.js --lang $(L)
else ifeq ($(I),ubuntu)
	docker pull ubuntu:rolling
	hash="$$(docker inspect ubuntu:rolling | jq '.[0].Id' -r | sha1sum | awk '{ print $$1 }')"; echo "FROM ubuntu:rolling" | docker build --label riju.image-hash="$${hash}" -t riju:$(I) -
else ifneq (,$(filter $(I),admin ci))
	docker build . -f docker/$(I)/Dockerfile -t riju:$(I) $(NO_CACHE)
else
	hash="$$(node tools/hash-dockerfile.js $(I) | grep .)"; docker build . -f docker/$(I)/Dockerfile -t riju:$(I) --label riju.image-hash="$${hash}" $(NO_CACHE)
endif

VOLUME_MOUNT ?= $(PWD)

P1 ?= 6119
P2 ?= 6120

ifneq (,$(E))
SHELL_PORTS := -p 127.0.0.1:$(P1):6119 -p 127.0.0.1:$(P2):6120
else
SHELL_PORTS :=
endif

SHELL_ENV := -e Z -e CI -e TEST_PATIENCE -e TEST_CONCURRENCY

ifeq ($(I),lang)
LANG_TAG := lang-$(L)
else
LANG_TAG := $(I)
endif

IMAGE_HASH := -e RIJU_IMAGE_HASH="$$(docker inspect riju:$(LANG_TAG) | jq '.[0].Config.Labels["riju.image-hash"]' -r)"

shell: # I=<shell> [L=<lang>] [E=1] [P1|P2=<port>] : Launch Docker image with shell
	@: $${I}
ifneq (,$(filter $(I),admin ci))
	@mkdir -p $(HOME)/.aws $(HOME)/.docker $(HOME)/.ssh $(HOME)/.terraform.d
	docker run -it --rm --hostname $(I) -v $(VOLUME_MOUNT):/src -v /var/run/docker.sock:/var/run/docker.sock -v $(HOME)/.aws:/var/riju/.aws -v $(HOME)/.docker:/var/riju/.docker -v $(HOME)/.ssh:/var/riju/.ssh -v $(HOME)/.terraform.d:/var/riju/.terraform.d -e AWS_REGION -e AWS_ACCESS_KEY_ID -e AWS_SECRET_ACCESS_KEY -e DOCKER_USERNAME -e DOCKER_PASSWORD -e DEPLOY_SSH_PRIVATE_KEY -e DOCKER_REPO -e S3_BUCKET -e DOMAIN -e VOLUME_MOUNT=$(VOLUME_MOUNT) $(SHELL_PORTS) $(SHELL_ENV) $(IMAGE_HASH) --network host riju:$(I) $(BASH_CMD)
else ifeq ($(I),app)
	docker run -it --rm --hostname $(I) $(SHELL_PORTS) $(SHELL_ENV) $(IMAGE_HASH) riju:$(I) $(BASH_CMD)
else ifneq (,$(filter $(I),base lang))
ifeq ($(I),lang)
	@: $${L}
endif
	docker run -it --rm --hostname $(LANG_TAG) -v $(VOLUME_MOUNT):/src --label riju-install-target=yes $(SHELL_PORTS) $(SHELL_ENV) $(IMAGE_HASH) riju:$(LANG_TAG) $(BASH_CMD)
else ifeq ($(I),runtime)
	docker run -it --rm --hostname $(I) -v $(VOLUME_MOUNT):/src -v /var/run/docker.sock:/var/run/docker.sock $(SHELL_PORTS) $(SHELL_ENV) $(IMAGE_HASH) riju:$(I) $(BASH_CMD)
else
	docker run -it --rm --hostname $(I) -v $(VOLUME_MOUNT):/src $(SHELL_PORTS) $(SHELL_ENV) $(IMAGE_HASH) riju:$(I) $(BASH_CMD)
endif

## This is equivalent to 'make pkg' in a fresh packaging container
## followed by 'make install' in a persistent runtime container.

repkg: script # L=<lang> T=<type> : Build fresh .deb and install into live container
	@: $${L} $${T}
	$(MAKE_QUIETLY) shell I=packaging CMD="make pkg L=$(L) T=$(T)"
	ctr="$$(docker container ls -f label="riju-install-target=yes" -l -q)"; test "$${ctr}" || (echo "no valid container is live"; exit 1); docker exec "$${ctr}" make install L=$(L) T=$(T)

### Build packaging scripts

script: # L=<lang> T=<type> : Generate a packaging script
	@: $${L} $${T}
	mkdir -p $(BUILD)
	node tools/generate-build-script.js --lang $(L) --type $(T) > $(BUILD)/build.bash
ifeq ($(T),lang)
	node tools/generate-build-script.js --lang $(L) --type install > $(BUILD)/install.bash
endif
	chmod +x $(BUILD)/build.bash

all-scripts: # Generate packaging scripts for all languages
	node tools/write-all-build-scripts.js

### Run packaging scripts

pkg-clean: # L=<lang> T=<type> : Set up fresh packaging environment
	@: $${L} $${T}
	rm -rf $(BUILD)/src $(BUILD)/pkg
	mkdir -p $(BUILD)/src $(BUILD)/pkg

pkg-build: # L=<lang> T=<type> : Run packaging script in packaging environment
	@: $${L} $${T}
	cd $(BUILD)/src && pkg="$(PWD)/$(BUILD)/pkg" src="$(PWD)/$(BUILD)/src" $(or $(BASH_CMD),../build.bash)

pkg-debug: # L=<lang> T=<type> : Launch shell in packaging environment
	@: $${L} $${T}
	$(MAKE_QUIETLY) pkg-build L=$(L) T=$(T) CMD=bash

Z ?= none

## Z is the compression type to use; defaults to none. Higher
## compression levels (gzip is moderate, xz is high) take much longer
## but produce much smaller packages.

pkg-deb: # L=<lang> T=<type> [Z=gzip|xz] : Build .deb from packaging environment
	@: $${L} $${T}
	fakeroot dpkg-deb --build -Z$(Z) $(BUILD)/pkg $(BUILD)/$(DEB)

## This is equivalent to the sequence 'pkg-clean', 'pkg-build', 'pkg-deb'.

pkg: pkg-clean pkg-build pkg-deb # L=<lang> T=<type> [Z=gzip|xz] : Build fresh .deb

### Install packages

install: # L=<lang> T=<type> : Install built .deb
	@: $${L} $${T}
	if [[ -z "$$(ls -A /var/lib/apt/lists)" ]]; then sudo apt update; fi
	DEBIAN_FRONTEND=noninteractive sudo -E apt reinstall -y ./$(BUILD)/$(DEB)

### Build and run application code

frontend: # Compile frontend assets for production
	npx webpack --mode=production

frontend-dev: # Compile and watch frontend assets for development
	watchexec -w webpack.config.cjs -w node_modules -r --no-environment -- "echo 'Running webpack...' >&2; npx webpack --mode=development --watch"

system: # Compile setuid binary for production
	./system/compile.bash

system-dev: # Compile and watch setuid binary for development
	watchexec -w system/src -n -- ./system/compile.bash

server: # Run server for production
	node backend/server.js

server-dev: # Run and restart server for development
	watchexec -w backend -r -n -- node backend/server.js

build: frontend system # Compile all artifacts for production

dev: # Compile, run, and watch all artifacts and server for development
	$(MAKE_QUIETLY) -j3 frontend-dev system-dev server-dev

### Application tools

## L is a language identifier or a comma-separated list of them, to
## filter tests by language. T is a test type (run, repl, lsp, format,
## etc.) or a set of them to filter tests that way. If both filters
## are provided, then only tests matching both are run.

test: # [L=<lang>[,...]] [T=<test>[,...]] : Run test(s) for language or test category
	node backend/test-runner.js $(L)

## Functions such as 'repl', 'run', 'format', etc. are available in
## the sandbox, and initial setup has already been done (e.g. 'setup'
## command, template code written to main).

sandbox: # L=<lang> : Run isolated shell with per-language setup
	@: $${L}
	L=$(L) node backend/sandbox.js

## L can be either a language ID or a (quoted) custom command to start
## the LSP. This does not run in a sandbox; the server is started
## directly in the current working directory.

lsp: # L=<lang|cmd> : Run LSP REPL for language or custom command line
	@: $${C}
	node backend/lsp-repl.js $(L)

### Fetch artifacts from registries

pull: # I=<image> : Pull last published Riju image from Docker Hub
	@: $${I} $${DOCKER_REPO}
	docker pull $(DOCKER_REPO):$(I)
	docker tag $(DOCKER_REPO):$(I) riju:$(I)

download: # L=<lang> T=<type> : Download last published .deb from S3
	@: $${L} $${T} $${S3_BUCKET}
	mkdir -p $(BUILD)
	aws s3 cp $(S3_DEB) $(BUILD)/$(DEB) --no-sign-request

### Publish artifacts to registries

push: # I=<image> : Push Riju image to Docker Hub
	@: $${I} $${DOCKER_REPO}
	docker tag riju:$(I) $(DOCKER_REPO):$(I)
	docker push $(DOCKER_REPO):$(I)

upload: # L=<lang> T=<type> : Upload .deb to S3
	@: $${L} $${T} $${S3_BUCKET}
	aws s3 rm --recursive $(S3_HASH)
	aws s3 cp $(BUILD)/$(DEB) $(S3_DEB)
	hash="$$(dpkg-deb -f $(BUILD)/$(DEB) Riju-Script-Hash | grep .)"; aws s3 cp - "$(S3_HASH)/$${hash}" < /dev/null

### Miscellaneous

## Run this every time you update .gitignore or .dockerignore.in.

dockerignore: # Update .dockerignore from .gitignore and .dockerignore.in
	echo "# This file is generated by 'make dockerignore', do not edit." > .dockerignore
	cat .gitignore | sed 's/#.*//' | grep . | sed 's#^#**/#' >> .dockerignore

## You need to be inside a 'make env' shell whenever you are running
## manual commands (Docker, Terraform, Packer, etc.) directly, as
## opposed to through the Makefile.

env: # Run shell with .env file loaded and $PATH fixed
	exec bash --rcfile <(cat ~/.bashrc - <<< 'PS1="[.env] $$PS1"')

tmux: # Start or attach to tmux session
	MAKELEVEL= tmux attach || MAKELEVEL= tmux new-session -s tmux

 usage:
	@cat Makefile | \
		grep -E '^[^.:[:space:]]+:|[#]##' | \
		sed -E 's/:[^#]*#([^:]+)$$/: #:\1/' | \
		sed -E 's/([^.:[:space:]]+):([^#]*#(.+))?.*/  make \1\3/' | \
		sed -E 's/[#][#]# *(.+)/\n    (\1)\n/' | \
		sed 's/$$/:/' | \
		column -ts:

help: # [CMD=<target>] : Show available targets, or detailed help for target
ifeq ($(CMD),)
	@echo "usage:"
	@make -s usage
	@echo
else
	@if ! make -s usage | grep -q "make $(CMD) "; then echo "no such target: $(CMD)"; exit 1; fi
	@echo "usage:"
	@make -s usage | grep "make $(CMD)"
	@echo
	@cat Makefile | \
		grep -E '^[^.:[:space:]]+:|^##[^#]' | \
		sed '/$(CMD):/Q' | \
		tac | \
		sed '/^[^#]/Q' | \
		tac | \
		sed -E 's/[# ]+//'
endif
