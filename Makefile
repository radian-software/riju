UID := $(shell id -u)

.PHONY: help
help: ## Show this message
	@echo "usage:" >&2
	@grep -h "[#]# " $(MAKEFILE_LIST)	| \
		sed 's/^/  make /'		| \
		sed 's/:[^#]*[#]# /|/'		| \
		column -t -s'|' >&2

.PHONY: docker
docker: ## Run shell with source code and deps inside Docker
	scripts/docker.bash build . -t fast-sandbox --build-arg "UID=$(UID)"
	scripts/docker.bash run -it --rm -v "$(PWD):/home/docker/src" -p 6119:6119 fast-sandbox
