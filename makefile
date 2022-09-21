base-name := decker
executable := $(shell stack path | grep local-install-root | sed "s/local-install-root: //")/bin/decker
version := $(shell grep "version: " package.yaml | sed "s/version: *//")
branch := $(shell git rev-parse --abbrev-ref HEAD)
commit := $(shell git rev-parse --short HEAD)
local-bin-path := $(HOME)/.local/bin

decker-name := $(base-name)-$(version)-$(branch)-$(commit)


build: 
	stack build -j8

clean-build: clean 
	git submodule update --init
	make -f symlinks.mk -C third-party all
	stack build -j8

less:
	stack build 2>&1 | less 

resource-zip:
	rm -f resource/decker-resources.zip
	(cd resource; zip -qr decker-resources.zip example support template)

install: clean-build
	mkdir -p $(local-bin-path)
	rm -f "$(local-bin-path)/$(base-name)" # work around codesign caching issue on Apple Silicon (https://openradar.appspot.com/FB8914243)
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)-$(version)

unclean-install: build
	mkdir -p $(local-bin-path)
	rm -f "$(local-bin-path)/$(base-name)" # work around codesign caching issue on Apple Silicon (https://openradar.appspot.com/FB8914243)
	cp $(executable) "$(local-bin-path)/$(decker-name)"
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)
	ln -sf "$(decker-name)" $(local-bin-path)/$(base-name)-$(version)

install-link: build
	mkdir -p $(local-bin-path)
	rm "$(local-bin-path)/$(base-name)-dev"
	ln -s $(executable) "$(local-bin-path)/$(base-name)-dev"

version:
	@echo "$(decker-name)"

build-profile:
	stack build --work-dir .stack-work-profile --profile

profile: build-profile
	stack exec -- decker clean
	stack exec --work-dir .stack-work-profile -- decker +RTS -p

dist: install
	rm -rf dist
	mkdir -p dist
	ln -s $(executable) dist/$(decker-name)
	zip -qj dist/$(decker-name).zip dist/$(decker-name)
	rm dist/$(decker-name)

test:
	stack test -j1

documentation:
	stack haddock

watch:
	stack test -j1 --file-watch

server:
	stack run -- decker --server --port 8888 --bind localhost html

clean:
	stack clean
	rm -rf dist public
	rm -rf resource/decker/support/vendor

append-webm:
	curl -T test/decks/movie.webm http://localhost:8888/append/test/decks/media-recording.webm
	# ls -rtl test/decks/media-recording*

replace-webm:
	curl -T test/decks/movie.webm http://localhost:8888/replace/test/decks/media-recording.webm
	curl -T test/decks/movie-1.webm http://localhost:8888/replace/test/decks/media-recording.webm
	# ls -rtl test/decks/media-recording*

clean-recordings:
	rm -f test/decks/*-recording*
	rm -f test/decks/*-times.json
	rm -f test/decks/*-annot.json

.PHONY: build clean test install dist docs resource-zip css
