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
	scripts/docker.bash run -it --rm -v "$(PWD):/home/docker/src" -p 6119:6119 riju bash

.PHONY: deploy
deploy: image-prod ## Deploy to Heroku
	scripts/docker.bash tag riju:prod registry.heroku.com/riju-sandbox/web
	heroku auth:token | scripts/docker.bash login --username=_ --password-stdin registry.heroku.com
	scripts/docker.bash push registry.heroku.com/riju-sandbox/web
	heroku container:release web -a riju-sandbox
