top_srcdir=..\..
ECLDIR=../package/
ECL=..\package\ecl.exe

all: show-fails

.PHONY: do-regressions cleanup clean-sources update

config.lsp: $(top_srcdir)\src\tests\config.lsp.in ..\c\cut.exe Makefile
	..\c\cut.exe "@builddir@" "./" \
	          "@top_srcdir@" "../.." \
	  < $(top_srcdir)\src\tests\config.lsp.in > config.lsp

output.ecl\regressions.log: config.lsp
	$(MAKE) do-regressions

do-regressions: regressions config.lsp
	set ECLDIR=$(ECLDIR)
	$(ECL) -norc -load config.lsp -eval "(ecl-tests::run-regressions-tests)" -eval "(ext:quit)"

show-fails: regressions.log
	type regressions.log

#
# Create directories
#
regressions.log: config.lsp
	$(MAKE) do-regressions

#
# Cleanup
#
clean:
	rm -rf output*

clean-sources:
	test -f config.lsp.in || rm -rf bugs
	rm -rf ansi-tests quicklisp

distclean: clean-sources clean
	rm -rf cache

update: clean-sources
	$(MAKE) regressions

