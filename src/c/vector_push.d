/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    string.d -- String routines.
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under thep terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/


#include <ecl/ecl.h>
#include <ecl/internal.h>

static cl_object
extend_vector(cl_object v, cl_index amount)
{
	cl_object other;
	cl_index new_length;
	unlikely_if (!ECL_VECTORP(v)) {
		FEwrong_type_nth_arg(@[vector-push-extend],1,v,@[vector]);
	}
	if (!ECL_ADJUSTABLE_ARRAY_P(v))
		FEerror("vector-push-extend: the array ~S is not adjustable.",
			1, v);
	if (v->vector.dim >= ADIMLIM)
		FEerror("Can't extend the array.", 0);
	if (amount == 0)
		amount = v->vector.dim / 2 + 1;
	new_length = v->vector.dim + amount;
	if (new_length > ADIMLIM)
		new_length = ADIMLIM;
	other = si_make_vector(cl_array_element_type(v),
			       MAKE_FIXNUM(new_length), Ct,
			       MAKE_FIXNUM(v->vector.fillp),
			       Cnil, MAKE_FIXNUM(0));
	ecl_copy_subarray(other, 0, v, 0, v->vector.fillp);
	return si_replace_array(v, other);
}

ecl_character
ecl_string_push_extend(cl_object s, ecl_character c)
{
	switch(type_of(s)) {
#ifdef ECL_UNICODE
	case t_string:
#endif
	case t_base_string:
		/* We use the fact that both string types are
		   byte-compatible except for the data. */
		if (s->base_string.fillp >= s->base_string.dim) {
			s = extend_vector(s, 0);
		}
		ecl_char_set(s, s->base_string.fillp++, c);
		return c;
	default:
                FEwrong_type_nth_arg(@[vector-push-extend],1,s,@[string]);
	}
}

cl_object
cl_vector_push(cl_object value, cl_object v)
{
	cl_index f = fix(cl_fill_pointer(v));
	if (f >= v->vector.dim) {
		@(return Cnil);
	} else {
		ecl_aset1(v, v->vector.fillp, value);
		@(return MAKE_FIXNUM(v->vector.fillp++));
	}
}

@(defun vector-push-extend (value v &optional (extent MAKE_FIXNUM(0)))
@
	cl_index f = fix(cl_fill_pointer(v));
	if (f >= v->vector.dim) {
		v = extend_vector(v, ecl_to_size(extent));
	}
	ecl_aset1(v, v->vector.fillp, value);
	@(return MAKE_FIXNUM(v->vector.fillp++));
@)
