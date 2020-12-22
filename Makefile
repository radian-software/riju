export PATH := bin:$(PATH)

.PHONY: debug
debug:
	node builder/build.js python

.PHONY: packaging-image
packaging-image:
	docker build . -f docker/packaging/Dockerfile -t riju:packaging

.PHONY: runtime-image
runtime-image:
	docker build . -f docker/runtime/Dockerfile -t riju:runtime

.PHONY: app-image
app-image:
	docker build . -f docker/app/Dockerfile -t riju:app

.PHONY: packaging-shell
packaging-shell:
	docker run -it --rm -v $(PWD):/src riju:packaging

.PHONY: runtime-shell
runtime-shell:
	docker run -it --rm -v $(PWD):/src riju:runtime

.PHONY: pkg
pkg:
	node src/packager/main.js --lang $(LANG)
