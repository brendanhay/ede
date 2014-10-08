SHELL         := /usr/bin/env bash
NAME          := ede
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox

CONFIGURED    := dist/setup-config
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
BIN           := dist/build/$(NAME)/$(NAME)
IMG           := dist/image/$(NAME)

FLAGS         := --disable-documentation --disable-library-coverage

.PHONY: test bench lint doc dist deps

all: build

build: $(CONFIGURED)
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: cabal.sandbox.config
	cabal install $(FLAGS)

dist: $(DEB)
	cabal sdist

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test: $(CONFIGURED)
	cabal test

bench: $(CONFIGURED)
	cabal bench

lint:
	hlint src

doc:
	cabal haddock

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(CABAL_SANDBOX)

deps: cabal.sandbox.config
	cabal install -j --only-dependencies --enable-tests --enable-benchmarks

$(CONFIGURED): deps cabal.sandbox.config $(NAME).cabal
	cabal configure --bindir=bin --libdir=lib --enable-tests --enable-benchmarks

$(BIN): $(CONFIGURED) test
	cabal build -j

$(IMG): $(BIN)
	cabal copy --destdir=dist/image

%.deb: $(IMG)
	makedeb --name=$(NAME) \
          --version=$(VERSION) \
          --debian-dir=deb \
          --build=$(BUILD_NUMBER) \
          --architecture=amd64 \
          --output-dir=dist
