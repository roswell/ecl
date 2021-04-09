#
#			Makefile for ECL core library
#
top_srcdir = ..\..\src
srcdir	= ..\..\src\c

!if "$(ECL_WIN64)" != ""
ECL_FPE_CODE=fpe_none.c
!else
ECL_FPE_CODE=fpe_x86.c
!endif

!if "$(ECL_THREADS)" != ""
ECL_THREADS_FLAG=1
THREADS_OBJ= process.obj mutex.obj condition_variable.obj rwlock.obj \
             semaphore.obj barrier.obj mailbox.obj atomic.obj queue.obj
!else
ECL_THREADS_FLAG=0
THREADS_OBJ=
!endif

!if "$(ECL_UNICODE)" != ""
ECL_UNICODE_FLAG=21
ECL_UCD_OBJ = ucd.obj ucd-0000.obj ucd-0016.obj \
	      ucd-0032.obj ucd-0048.obj ucd-0064.obj \
	      ucd-0080.obj ucd-0096.obj ucd_names_char.obj ucd_names_codes.obj ucd_names_pair.obj ucd_names_str.obj

!else
ECL_UNICODE_FLAG=0
!endif

!if "$(ECL_SSE)" != ""
ECL_SSE_FLAG=1
ECL_SSE_OBJ=sse2.obj
!else
ECL_SSE_FLAG=0
ECL_SSE_OBJ=
!endif

!if "$(ECL_USE_DBGHELP)" != ""
ECL_USE_DBGHELP_FLAG=1
!else
ECL_USE_DBGHELP_FLAG=0
!endif

# Programs used by "make":
#
CC	= cl
CFLAGS	= -c $(ECL_CFLAGS) -DECL_BUILD -DECL_API="__declspec(dllexport)" -I./ -I../ -I../ecl -I$(srcdir) -I$(srcdir)/unicode

SHELL	= /bin/sh
RM	= del
CP      = copy /Y
MV      = move /Y
LINK    = link
EXE	= .exe
DPP	= .\dpp$(EXE)

# Data for installation
#
INSTALL = @INSTALL@
INSTALL_DATA = @INSTALL_DATA@
prefix=@prefix@
exec_prefix=$(prefix)
libdir=$(prefix)\lib\ecl

# Files

HDIR	= $(top_srcdir)\h
HFILES	= ..\ecl\config.h ..\ecl\config-internal.h ..\ecl\atomic_ops.h \
	$(HDIR)\ecl.h $(HDIR)\ecl-cmp.h \
	$(HDIR)\object.h $(HDIR)\cs.h $(HDIR)\stacks.h \
	$(HDIR)\external.h $(HDIR)\cons.h $(HDIR)\legacy.h \
	$(HDIR)\number.h $(HDIR)\page.h \
	$(HDIR)\internal.h $(HDIR)\ecl-inl.h $(HDIR)\bytecodes.h \
	$(HDIR)\impl\math_dispatch.h $(HDIR)\impl\math_dispatch2.h \
	$(HDIR)\impl\math_fenv.h $(HDIR)\impl\math_fenv_msvc.h \
	$(HDIR)\cache.h $(HDIR)\stack-resize.h \
	$(HDIR)\ecl_atomics.h

OBJS	= main.obj symbol.obj package.obj cons.obj list.obj\
	apply.obj eval.obj \
	interpreter.obj compiler.obj disassembler.obj \
	instance.obj gfun.obj cache.obj accessor.obj \
	reference.obj character.obj\
	file.obj read.obj print.obj error.obj string.obj cfun.obj\
	parse_integer.obj parse_number.obj \
	float_to_digits.obj float_to_string.obj \
	integer_to_string.obj write_ugly.obj \
	write_object.obj write_symbol.obj \
	write_array.obj write_list.obj write_code.obj \
	write_sse.obj print_unreadable.obj \
	libraries.obj backtrace.obj mmap.obj cdata.obj \
	cos.obj sin.obj tan.obj atan.obj \
	cosh.obj sinh.obj tanh.obj \
	exp.obj expt.obj log.obj \
	sqrt.obj abs.obj \
	zerop.obj plusp.obj minusp.obj \
	negate.obj conjugate.obj \
	one_plus.obj one_minus.obj \
	plus.obj minus.obj times.obj divide.obj \
	number_compare.obj number_equalp.obj minmax.obj \
	floor.obj ceiling.obj round.obj truncate.obj \
	typespec.obj assignment.obj \
	predicate.obj big.obj number.obj\
	num_pred.obj num_arith.obj num_co.obj\
	num_log.obj num_rand.obj array.obj vector_push.obj \
	sequence.obj cmpaux.obj\
	macros.obj backq.obj stacks.obj \
	time.obj unixint.obj\
	mapfun.obj multival.obj hash.obj format.obj pathname.obj\
	structure.obj load.obj unixfsys.obj unixsys.obj \
	ffi.obj alloc_2.obj tcp.obj $(THREADS_OBJ) serialize.obj \
	$(ECL_UCD_OBJ) $(ECL_SSE_OBJ)

all:	$(DPP) ..\eclmin.lib ..\cinit.obj

.SUFFIXES: .obj .c .d

{$(srcdir:\=/)}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/arch}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/clos/}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/reader}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/printer}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/ffi}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/unicode}.c{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/numbers}.d{}.c:
	$(DPP) $< $@
{$(srcdir:\=/)/threads}.d{}.c:
	$(DPP) $< $@

#.d.c:
#	$(DPP) $< $@
.c.obj:
	$(CC) $(CFLAGS) /Fo$@ $<

.PHONY:	all

cut.exe: $(top_srcdir)/util/cut.c
	$(CC) $(LDFLAGS) /Fecut.exe $(top_srcdir)/util/cut.c

..\eclmin.lib: $(OBJS:.obj=.c) all_symbols.c $(OBJS) all_symbols.obj all_symbols2.obj
	-$(RM) $@
	$(LINK) -lib -nologo -out:$@ $(OBJS)

clean:
	-for %f in ($(OBJS:.obj=.c) all_symbols.c) do $(RM) %f
	-for %f in ($(OBJS) all_symbols.obj all_symbols2.obj) do $(RM) %f
	-$(RM) *.pdb
	-for %f in (..\ecl\config.h dpp dpp.obj $(DPP) cut.exe cut.obj \
	            ..\eclmin.lib ..\cinit.obj cinit.c) \
	     do $(RM) %f
	-for %f in (..\ecl\*.h) do $(RM) %f
	-for %f in (cut.exe cut.obj dpp.exe dpp.obj) do $(RM) %f
	-del /S /Q ..\ecl\atomic_ops
	-del /S /Q ..\ecl\impl

# Build rules

$(DPP): $(srcdir)/dpp.c $(srcdir)/symbols_list.h ../ecl/config.h ../ecl/config-internal.h
	$(CC) /nologo -I.. -I./ $(srcdir)/dpp.c /Fe$@
$(HFILES): ../ecl/config.h.msvc6 ../ecl/config-internal.h.msvc6 Makefile
	-mkdir ..\ecl\impl
	cut.exe "@ECL_FPE_CODE@" "$(srcdir:\=/)/arch/$(ECL_FPE_CODE)" \
		"@ECL_VERSION_NUMBER@" "$(ECL_VERSION_NUMBER)" \
                "@ECL_DEFAULT_C_STACK_SIZE@" "$(ECL_DEFAULT_C_STACK_SIZE)" \
		"@ECL_THREADS@" "$(ECL_THREADS_FLAG)" \
		"@ECL_UNICODE@" "$(ECL_UNICODE_FLAG)" \
		"@ECL_SSE2@" "$(ECL_SSE_FLAG)" \
		"@ECL_USE_DBGHELP@" "$(ECL_USE_DBGHELP_FLAG)" \
	        < ..\ecl\config.h.msvc6 > ..\ecl\config.h
	cut.exe "@ECL_FPE_CODE@" "$(srcdir:\=/)/arch/$(ECL_FPE_CODE)" \
		"@ECL_VERSION_NUMBER@" "$(ECL_VERSION_NUMBER)" \
                "@ECL_DEFAULT_C_STACK_SIZE@" "$(ECL_DEFAULT_C_STACK_SIZE)" \
		"@ECL_THREADS@" "$(ECL_THREADS_FLAG)" \
		"@ECL_UNICODE@" "$(ECL_UNICODE_FLAG)" \
		"@ECL_SSE2@" "$(ECL_SSE_FLAG)" \
		"@ECL_USE_DBGHELP@" "$(ECL_USE_DBGHELP_FLAG)" \
		"@ARCHITECTURE@" "$(ECL_ARCHITECTURE)" \
	        < ..\ecl\config-internal.h.msvc6 > ..\ecl\config-internal.h
	xcopy /SYI $(top_srcdir)\h\*.h ..\ecl
	-mkdir ..\ecl\atomic_ops
	-mkdir ..\ecl\atomic_ops\sysdeps
	xcopy /SYI $(top_srcdir)\bdwgc\libatomic_ops\src\atomic_ops.h ..\ecl
	xcopy /SYI $(top_srcdir)\bdwgc\libatomic_ops\src\atomic_ops\generalize*.h ..\ecl\atomic_ops
	xcopy /SYI $(top_srcdir)\bdwgc\libatomic_ops\src\atomic_ops\sysdeps\* ..\ecl\atomic_ops\sysdeps
	xcopy /SYI $(top_srcdir)\bdwgc\libatomic_ops\src\atomic_ops\ao_version.h ..\ecl\atomic_ops

#
# GCC might break this code
#
gbc.o: gbc.c $(HFILES)
	$(CC) $(CFLAGS) -O0 gbc.c /Fo$@
#
# This reduces the overhead of jumping to other functions
#
apply.o: apply.c $(HFILES) $(HDIR)/cs.h
	$(CC) $(CFLAGS) apply.c /Fo$@
#
# These files are interrelated
#
all_symbols.obj: all_symbols.c
	$(CC) $(CFLAGS) -Fo$@ -I../ -I./ all_symbols.c
all_symbols2.obj: all_symbols.c
	$(CC) $(CFLAGS) -DECL_FINAL -Fo$@ -I../ -I./ all_symbols.c
#
# This is in another directory
#
../cinit.obj: cinit.c
	$(CC) $(CFLAGS) -I../ -I./ /Focinit.obj cinit.c
	$(MV) cinit.obj ..\
