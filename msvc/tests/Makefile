top_srcdir=..\..
ECLDIR=../package/
ECL=..\package\ecl.exe

.PHONY: all

all: check

check: config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc \
                -load config.lsp \
                -eval "(ecl-tests::run-tests '($(TESTS)))" \
                -eval "(ext:quit)"

config.lsp: $(top_srcdir)\src\tests\config.lsp.in ..\c\cut.exe Makefile
	..\c\cut.exe "@builddir@" "./" \
	          "@true_srcdir@" "../src" \
	  < $(top_srcdir)\src\tests\config.lsp.in > config.lsp
