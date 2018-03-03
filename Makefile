default:

all: configure install-dependencies build doc test

configure:
	cabal configure --enable-tests -f development

install-dependencies: .cabal-sandbox/
	cabal install --enable-tests --dependencies-only

build:
	cabal build

test:
	cabal test --show-details=always

doc: dist/doc/html/tree-traversals dist/doc/html/README.html

.cabal-sandbox:
	cabal sandbox init

dist/doc/html/tree-traversals: src/*.hs
	cabal haddock
	touch $@

dist/doc/html/README.html: README.md dist/doc/html/gfm.css
	pandoc $< -o $@ --css gfm.css --standalone --from gfm --to html --metadata=title:README

dist/doc/html/gfm.css: gfm.css
	mkdir -p $(dir $@)
	cp $< $@

.PHONY: default all example install-dependencies build doc test
