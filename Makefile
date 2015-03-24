SHELL := /usr/bin/env bash

.PHONY: bench test

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install -j \
 --disable-documentation \
 --only-dependencies

cabal.sandbox.config:
	cabal sandbox init

bench:
	cabal install --enable-benchmarks && \
 cabal bench --benchmark-option=-obenchmark.html

test:
	cabal install --enable-tests && \
 cabal test

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox benchmark.html

doc:
	cabal haddock
