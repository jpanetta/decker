executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
base-name := decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git branch | grep \* | cut -d ' ' -f2)
local-bin-path := ~/.local/bin

ifeq ($(branch),master)
	decker-name := $(base-name)-$(version)
else
	decker-name := $(base-name)-$(version)-$(branch)
endif

build:
	stack build -j 8 --fast

yarn:
	yarn install && yarn run webpack --mode production
	cp -r node_modules/reveal.js-menu resource/support/

dist: yarn build
	rm -rf dist
	mkdir -p dist
	ln -s $(executable) dist/$(decker-name) 
	zip -qj dist/$(decker-name).zip dist/$(decker-name)
	rm dist/$(decker-name)

test:
	stack test -j 8 --fast

watch:
	stack test -j 8 --fast --file-watch

clean:
	stack clean
	rm -rf dist

install: yarn build
	stack exec -- decker clean
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)

version:
	@echo "$(decker-name)"

.PHONY: build clean test install dist docs yarn
