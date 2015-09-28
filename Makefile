# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-

GIT_VERSION=$(shell git describe --tags --always)

DIST_NAME=study-paip#-$(GIT_VERSION)
DIST_FILE=dist/$(DIST_NAME).zip

LISP=sbcl
SBCL_OPTIONS=--dynamic-space-size 24000 --noinform        \
             --lose-on-corruption --end-runtime-options   \
             --non-interactive --no-userinit --no-sysinit \
             --disable-debugger

CL ?= $(LISP) $(SBCL_OPTIONS)

.PHONY: clean
clean:
	find . -name '*~' -o -name '*.fasl' | xargs rm -f

.PHONY: distclean
distclean: clean
	rm -rf dist/

.PHONY: dist
dist:
	-mkdir dist
	git archive --format=zip --prefix "$(DIST_NAME)/" HEAD > "$(DIST_FILE)"
	mkdir "$(DIST_NAME)/"
	zip -r "$(DIST_FILE)" "$(DIST_NAME)/"
	rm -rf "$(DIST_NAME)"
