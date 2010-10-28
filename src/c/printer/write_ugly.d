/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    print.d -- Print.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifndef _MSC_VER
# include <unistd.h>
#endif
#include <ecl/ecl.h>
#include <ecl/internal.h>

#define call_print_object(x,s)	funcall(3, @'print-object',(x),(s))
#define call_structure_print_function(f,x,s) funcall(4,(f),(x),(s),MAKE_FIXNUM(0))

static void
write_readable_pathname(cl_object path, cl_object stream)
{
        cl_object l =
                cl_list(15, @'make-pathname',
                        @':host', path->pathname.host,
                        @':device', path->pathname.device,
                        @':directory',
                        cl_funcall(2, @'ext::maybe-quote', path->pathname.directory),
                        @':name', path->pathname.name,
                        @':type', path->pathname.type,
                        @':version', path->pathname.version,
                        @':defaults', Cnil);
        writestr_stream("#.", stream);
        si_write_object(l, stream);
}

static void
write_pathname(cl_object path, cl_object stream)
{
        cl_object namestring = ecl_namestring(path, 0);
        bool readably = ecl_print_readably();
        if (namestring == Cnil) {
                if (readably) {
                        write_readable_pathname(path, stream);
                        return;
                }
                namestring = ecl_namestring(path, 1);
                if (namestring == Cnil) {
                        writestr_stream("#<Unprintable pathname>", stream);
                        return;
                }
        }
        if (readably || ecl_print_escape())
                writestr_stream("#P", stream);
        si_write_ugly_object(namestring, stream);
}

static void
write_integer(cl_object number, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        int print_base = ecl_print_base();
        si_integer_to_string(s, number,
                             MAKE_FIXNUM(print_base),
                             ecl_symbol_value(@'*print-radix*'),
                             Ct /* decimal syntax */);
        si_do_write_sequence(s, stream, MAKE_FIXNUM(0), Cnil);
        si_put_buffer_string(s);
}

void
_ecl_write_fixnum(cl_fixnum i, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        si_integer_to_string(s, MAKE_FIXNUM(i), MAKE_FIXNUM(10), Cnil, Cnil);
        si_do_write_sequence(s, stream, MAKE_FIXNUM(0), Cnil);
        si_put_buffer_string(s);
}

static void
write_ratio(cl_object r, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        int print_base = ecl_print_base();
        si_integer_to_string(s, r->ratio.num, MAKE_FIXNUM(print_base),
                             ecl_symbol_value(@'*print-radix*'),
                             Cnil /* decimal syntax */);
        ecl_string_push_extend(s, '/');
        si_integer_to_string(s, r->ratio.den,
                             MAKE_FIXNUM(print_base),
                             Cnil, Cnil);
        si_do_write_sequence(s, stream, MAKE_FIXNUM(0), Cnil);
        si_put_buffer_string(s);
}

static void
write_complex(cl_object x, cl_object stream)
{
        writestr_stream("#C(", stream);
        si_write_ugly_object(x->complex.real, stream);
        ecl_write_char(' ', stream);
        si_write_ugly_object(x->complex.imag, stream);
        ecl_write_char(')', stream);
}

static void
write_float(cl_object f, cl_object stream)
{
        cl_object s = si_get_buffer_string();
        s = si_float_to_string_free(s, f, MAKE_FIXNUM(-3), MAKE_FIXNUM(8));
        si_do_write_sequence(s, stream, MAKE_FIXNUM(0), Cnil);
        si_put_buffer_string(s);
}

static void
write_character(cl_object x, cl_object stream)
{
        int i = CHAR_CODE(x);
	if (!ecl_print_escape() && !ecl_print_readably()) {
		ecl_write_char(i, stream);
	} else {
		writestr_stream("#\\", stream);
		if (i < 32 || i == 127) {
			cl_object name = cl_char_name(CODE_CHAR(i));
			writestr_stream((char*)name->base_string.self, stream);
		} else if (i >= 128) {
                        int  index = 0;
			char name[20];
			sprintf(name, "U%04x", i); /* cleanup */
                        while(name[index])
				ecl_write_char(name[index++], stream);
		} else {
			ecl_write_char(i, stream);
		}
	}
}

static void
write_free(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "free", Cnil, stream);
}

static void
write_hashtable(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "hash-table", Cnil, stream);
}

static void
write_random(cl_object x, cl_object stream)
{
        if (ecl_print_readably()) {
                writestr_stream("#$", stream);
                _ecl_write_vector(x->random.value, stream);
        } else {
                _ecl_write_unreadable(x->random.value, "random-state", Cnil, stream);
        }
}

static void
write_stream(cl_object x, cl_object stream)
{
        const char *prefix;
        cl_object tag;
#ifdef ECL_UNICODE
        ecl_character buffer[20];
#else
        ecl_base_char buffer[20];
#endif
        union cl_lispunion str;
        switch ((enum ecl_smmode)x->stream.mode) {
        case smm_input_file:
        case smm_input:
                prefix = "closed input stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case smm_output_file:
        case smm_output:
                prefix = "closed output stream";
                tag = IO_STREAM_FILENAME(x);
                break;
#ifdef ECL_MS_WINDOWS_HOST
        case smm_input_wsock:
                prefix = "closed input win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case smm_output_wsock:
                prefix = "closed output win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case smm_io_wsock:
                prefix = "closed i/o win32 socket stream";
                tag = IO_STREAM_FILENAME(x);
                break;
#endif
        case smm_io_file:
        case smm_io:
                prefix = "closed io stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case smm_probe:
                prefix = "closed probe stream";
                tag = IO_STREAM_FILENAME(x);
                break;
        case smm_synonym:
                prefix = "closed synonym stream to";
                tag = SYNONYM_STREAM_SYMBOL(x);
                break;
        case smm_broadcast:
                prefix = "closed broadcast stream";
                tag = Cnil;
                break;
        case smm_concatenated:
                prefix = "closed concatenated stream";
                tag = Cnil;
                break;
        case smm_two_way:
                prefix = "closed two-way stream";
                tag = Cnil;
                break;
        case smm_echo:
                prefix = "closed echo stream";
                tag = Cnil;
                break;
        case smm_string_input: {
                cl_object text = x->stream.object0;
                cl_index ndx, l = ecl_length(text);
                for (ndx = 0; (ndx < 8) && (ndx < l); ndx++) {
                        buffer[ndx] = ecl_char(text, ndx);
                }
                if (l > ndx) {
                        buffer[ndx-1] = '.';
                        buffer[ndx-2] = '.';
                        buffer[ndx-3] = '.';
                }
                buffer[ndx++] = 0;
                prefix = "closed string-input stream from";
                tag = &str;
#ifdef ECL_UNICODE
                tag->string.t = t_string;
                tag->string.self = buffer;
#else
                tag->base_string.t = t_base_string;
                tag->base_string.self = buffer;
#endif
                tag->base_string.dim = ndx;
                tag->base_string.fillp = ndx-1;
                break;
        }
        case smm_string_output:
                prefix = "closed string-output stream";
                tag = Cnil;
                break;
        default:
                ecl_internal_error("illegal stream mode");
        }
        if (!x->stream.closed)
                prefix = prefix + 7;
        _ecl_write_unreadable(x, prefix, tag, stream);
}

#ifndef CLOS
static void
write_structure(cl_object x, cl_object stream)
{
        cl_object print_function;
        unlikely_if (type_of(x->str.name) != t_symbol)
                FEerror("Found a corrupt structure with an invalid type name~%"
                        "  ~S", x->str.name);
        print_function = si_get_sysprop(x->str.name, @'si::structure-print-function');
        if (Null(print_function) || !ecl_print_structure()) {
                writestr_stream("#S", stream);
                /* structure_to_list conses slot names and values into
                 * a list to be printed.  print shouldn't allocate
                 * memory - Beppe
                 */
                x = structure_to_list(x);
                si_write_object(x, stream);
        } else {
                call_structure_print_function(print_function, x, stream);
        }
}
#endif /* !CLOS */

static void
write_readtable(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "readtable", Cnil, stream);
}

static void
write_foreign(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "foreign", x->foreign.tag, stream);
}

static void
write_frame(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "frame", MAKE_FIXNUM(x->frame.size), stream);
}

static void
write_weak_pointer(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "weak-pointer", Cnil, stream);
}

#ifdef ECL_THREADS
static void
write_process(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "process", x->process.name, stream);
}

static void
write_lock(cl_object x, cl_object stream)
{
        const char *prefix = x->lock.recursive?
                "lock" : "lock (nonrecursive)";
        _ecl_write_unreadable(x, prefix, x->lock.name, stream);
}

static void
write_condition_variable(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "semaphore", Cnil, stream);
}

# ifdef ECL_SEMAPHORES
static void
write_semaphore(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "semaphore", Cnil, stream);
}
# endif
#endif /* ECL_THREADS */

static void
write_illegal(cl_object x, cl_object stream)
{
        _ecl_write_unreadable(x, "illegal pointer", Cnil, stream);
}

cl_object
si_write_ugly_object(cl_object x, cl_object stream)
{
	cl_object r, y;
	cl_fixnum i;
	cl_index ndx, k;

	if (x == OBJNULL) {
		if (ecl_print_readably())
                        FEprint_not_readable(x);
		writestr_stream("#<OBJNULL>", stream);
		goto OUTPUT;
	}
	switch (type_of(x)) {
	case FREE:
                write_free(x, stream);
		break;
	case t_fixnum:
	case t_bignum:
                write_integer(x, stream);
                break;
	case t_ratio:
                write_ratio(x, stream);
                break;
	case t_singlefloat:
	case t_doublefloat:
#ifdef ECL_LONG_FLOAT
	case t_longfloat:
#endif
                write_float(x, stream);
		break;
	case t_complex:
                write_complex(x, stream);
		break;
	case t_character:
		write_character(x, stream);
		break;
	case t_symbol:
		_ecl_write_symbol(x, stream);
		break;
	case t_array:
		_ecl_write_array(x, stream);
		break;
#ifdef ECL_UNICODE
	case t_string:
                _ecl_write_string(x, stream);
		break;
#endif
	case t_vector:
                _ecl_write_vector(x, stream);
		break;
	case t_base_string:
                _ecl_write_base_string(x, stream);
		break;
	case t_bitvector:
                _ecl_write_bitvector(x, stream);
		break;
	case t_list:
                _ecl_write_list(x, stream);
                break;
	case t_package:
		if (ecl_print_readably()) FEprint_not_readable(x);
		writestr_stream("#<", stream);
		si_write_ugly_object(x->pack.name, stream);
 		writestr_stream(" package>", stream);
		break;
	case t_hashtable:
                write_hashtable(x, stream);
		break;
	case t_stream:
                write_stream(x, stream);
		break;
	case t_random:
                write_random(x, stream);
		break;
#ifndef CLOS
	case t_structure:
                write_structure(x, stream);
                break;
#endif /* CLOS */
	case t_readtable:
                write_readtable(x, stream);
		break;
	case t_pathname:
                write_pathname(x, stream);
                break;
	case t_bclosure:
                _ecl_write_bclosure(x, stream);
                break;
	case t_bytecodes:
                _ecl_write_bytecodes(x, stream);
                break;
        case t_cfun:
	case t_cfunfixed:
		if (ecl_print_readably()) FEprint_not_readable(x);
		writestr_stream("#<compiled-function ", stream);
		if (x->cfun.name != Cnil)
			si_write_ugly_object(x->cfun.name, stream);
		else
			_ecl_write_addr(x, stream);
		ecl_write_char('>', stream);
		break;
	case t_codeblock:
		if (ecl_print_readably()) FEprint_not_readable(x);
		writestr_stream("#<codeblock ", stream);
		if (x->cblock.name != Cnil)
			si_write_ugly_object(x->cblock.name, stream);
		else
			_ecl_write_addr(x, stream);
		ecl_write_char('>', stream);
		break;
	case t_cclosure:
		if (ecl_print_readably()) FEprint_not_readable(x);
		writestr_stream("#<compiled-closure ", stream);
		_ecl_write_addr(x, stream);
		ecl_write_char('>', stream);
		break;
#ifdef CLOS
	case t_instance:
		call_print_object(x, stream);
		break;
#endif /* CLOS */
	case t_foreign:
                write_foreign(x, stream);
		break;
	case t_frame:
                write_frame(x, stream);
		break;
	case t_weak_pointer:
                write_weak_pointer(x, stream);
		break;
#ifdef ECL_THREADS
	case t_process:
                write_process(x, stream);
		break;
	case t_lock:
                write_lock(x, stream);
		break;
	case t_condition_variable:
                write_condition_variable(x, stream);
		break;
#endif /* ECL_THREADS */
#ifdef ECL_SEMAPHORES
	case t_semaphore:
                write_semaphore(x, stream);
		break;
#endif
#ifdef ECL_SSE2
	case t_sse_pack:
                _ecl_write_sse(x, stream);
		break;
#endif
	default:
                write_illegal(x, stream);
	}
 OUTPUT:
	@(return x)
}
