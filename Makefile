# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-

CL ?= sbcl --noinform --disable-debugger

clean:
	@find . -iname '*.fasl' -exec rm {} \;

.PHONY: clean
