SHELL        := /usr/bin/env bash
SANDBOX      ?= $(CURDIR)/.cabal-sandbox
NAME         := ede
VERSION      := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER ?= 0
DEB          := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
BIN          := dist/release/$(NAME)

.PHONY: install test doc

build: dist/setup-config
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

all:
	make clean; make install && make build

dist/setup-config: install
	cabal configure \
 --bindir=bin \
 --libdir=lib \
 --enable-tests \
 --enable-benchmarks

install: cabal.sandbox.config
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(SANDBOX)

clean:
	-rm -rf bin lib dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal install --enable-tests && \
 ./dist/build/golden/golden

doc:
	cabal haddock

dist: $(DEB)
	cabal sdist

$(BIN): build
	cabal copy --destdir=dist/release && upx $@

%.deb: $(BIN)
	makedeb --name=$(NAME) \
 --version=$(VERSION) \
 --debian-dir=deb \
 --build=$(BUILD_NUMBER) \
 --architecture=amd64 \
 --output-dir=dist
