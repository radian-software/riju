-include .env
export

UID := $(shell id -u)

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		column -t -s'|' >&2

.PHONY: image-dev
image-dev: ## Build Docker image for development
	scripts/docker.bash build . -f Dockerfile.dev -t riju --build-arg "UID=$(UID)"

.PHONY: image-prod
image-prod: ## Build Docker image for production
	scripts/docker.bash build . -f Dockerfile.prod -t riju:prod --build-arg "UID=$(UID)"

.PHONY: docker
docker: image-dev docker-nobuild ## Run shell with source code and deps inside Docker

.PHONY: docker-nobuild
docker-nobuild: ## Same as 'make docker', but don't rebuild image
	scripts/docker.bash run -it --rm -v "$(PWD):/home/docker/src" -p 6119:6119 -p 6120:6120 -h riju riju bash

.PHONY: deploy
deploy: image-prod ## Build, publish, and deploy production image
	scripts/deploy.bash
