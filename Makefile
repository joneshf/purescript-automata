BUILD := _build
DOCS := docs
SRCS := $(shell find src -name '*.purs' -type f)
TESTS := $(shell find test -name '*.purs' -type f)

.DEFAULT_GOAL := test

.PHONY: docs
docs: | bower_components/.stamp node_modules/.stamp
	npx pulp docs
	rm -fr $(DOCS)
	mv generated-docs $(DOCS)

bower_components/.stamp: bower.json | node_modules/.stamp
	npx bower install
	touch $@

.PHONY: build
build: $(SRCS) | bower_components/.stamp node_modules/.stamp
	npx pulp build

.PHONY: clean
clean:
	rm -fr \
	  $(DOCS) \
	  .pulp-cache \
	  bower_components \
	  generated-docs \
	  node_modules \
	  output

node_modules/.stamp: package.json package-lock.json
	npm install
	touch $@

.PHONY: test
test: $(SRCS) $(TESTS) | bower_components/.stamp node_modules/.stamp
	npx pulp test

.PHONY: watch
watch: | bower_components/.stamp node_modules/.stamp
	npx pscid --test
