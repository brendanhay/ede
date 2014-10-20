SHELL         := /usr/bin/env bash
CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox
FLAGS         := --enable-tests --enable-benchmarks -fbuild-executable
NAME          := ede
VERSION       := $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)
BUILD_NUMBER  ?= 0
DEB           := dist/$(NAME)_$(VERSION)+$(BUILD_NUMBER)_amd64.deb
BIN           := dist/image/bin/ede

.PHONY: test doc

build: dist/setup-config
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

all:
	make clean; make dist

dist/setup-config: install
	cabal configure $(FLAGS) --bindir=bin --libdir=lib

install: cabal.sandbox.config
	cabal install -j $(FLAGS) \
 --only-dependencies \
 --disable-documentation \
 --disable-library-coverage

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(CABAL_SANDBOX)

clean:
	-rm -rf bin lib dist cabal.sandbox.config .cabal-sandbox
	cabal clean

test:
	cabal test

doc:
	cabal haddock

dist: $(DEB)
	cabal sdist

$(BIN): build
	cabal copy --destdir=dist/image && upx $@

%.deb: $(BIN)
	makedeb --name=$(NAME) \
 --version=$(VERSION) \
 --debian-dir=deb \
 --build=$(BUILD_NUMBER) \
 --architecture=amd64 \
 --output-dir=dist
