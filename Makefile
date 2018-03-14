default:

all: install-dependencies configure build doc test

configure:
	cabal configure --enable-tests -f development

install-dependencies: .cabal-sandbox install-global-dependencies
	cabal install --enable-tests --dependencies-only

install-global-dependencies:
	which happy || ( cd .. && cabal install happy )
	which pandoc || ( cd .. && cabal install pandoc )

build:
	cabal build

test:
	cabal test --show-details=always

doc: dist/doc/html/tree-traversals dist/doc/html/README.html

.cabal-sandbox:
	cabal sandbox init

dist/doc/html/tree-traversals: $(shell find src -type f)
	cabal haddock
	touch $@

dist/doc/html/README.html: README.md dist/doc/html/gfm.css
	pandoc $< -o $@ --css gfm.css --standalone --from gfm --to html --metadata=title:README

dist/doc/html/gfm.css: gfm.css
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: default all example install-dependencies build doc test
