# -*- Mode: Makefile; tab-width: 4; indent-tabs-mode: t -*-

CL ?= sbcl --noinform --disable-debugger

clean:
	@find . -name ".fasls" | xargs rm -rf
	@find . \( -name "*.dfsl" -o -name "*.fasl" -o -name "*.fas" -o -name "*.lib" -o -name "*.x86f" -o -name "*.ppcf" -o -name "*.nfasl" -o -name "*.fsl" \) -exec rm {} \;

.PHONY: clean
