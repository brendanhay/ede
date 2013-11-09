SHELL := /usr/bin/env bash
FLAGS := -j --disable-documentation --disable-library-coverage

.PHONY: test bench lint doc

all: build

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install $(FLAGS)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests $(FLAGS)

bench:
	cabal install --enable-benchmarks $(FLAGS)

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init
