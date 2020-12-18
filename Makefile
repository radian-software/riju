export PATH := bin:$(PATH)

.PHONY: debug
debug:
	node builder/build.js python riju-lang-python.deb

.PHONY: build-image
build-image:
	docker build . -f docker/Dockerfile.build -t riju:build

.PHONY: build-shell
build-shell: build-image
	docker run -it --rm -v $(PWD):/src riju:build
