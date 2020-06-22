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
docker: image-dev ## Run shell with source code and deps inside Docker
	scripts/docker.bash run -it --rm -v "$(PWD):/home/docker/src" -p 6119:6119 -p 6120:6120 -h riju riju bash

.PHONY: deploy
deploy: ## Deploy current master from GitHub to production
	scripts/deploy.bash
