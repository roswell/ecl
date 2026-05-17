/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * eval.d - evaluation
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>

@(defun apply (fun lastarg &rest args)
@ {
    if (narg == 2 && ecl_t_of(lastarg) == t_frame) {
      return ecl_apply_from_stack_frame(lastarg, fun);
    } else {
      cl_object out;
      cl_index i;
      struct ecl_stack_frame frame_aux;
      const cl_object frame = ecl_stack_frame_open(the_env,
                                                   (cl_object)&frame_aux,
                                                   narg -= 2);
      for (i = 0; i < narg; i++) {
        ecl_stack_frame_push(frame, lastarg);
        lastarg = ecl_va_arg(args);
      }
      if (ecl_t_of(lastarg) == t_frame) {
        /* This could be replaced with a memcpy() */
        for (i = 0; i < lastarg->frame.size; i++) {
          ecl_stack_frame_push(frame, ECL_STACK_FRAME_REF(lastarg, i));
        }
      } else loop_for_in (lastarg) {
          if (ecl_unlikely(i >= ECL_CALL_ARGUMENTS_LIMIT)) {
            ecl_stack_frame_close(frame);
            FEprogram_error("CALL-ARGUMENTS-LIMIT exceeded",0);
          }
          ecl_stack_frame_push(frame, CAR(lastarg));
          i++;
        } end_loop_for_in;
      out = ecl_apply_from_stack_frame(frame, fun);
      ecl_stack_frame_close(frame);
      return out;
    }
}@)

cl_object
cl_eval(cl_object form)
{
  return si_eval_with_env(1, form);
}

@(defun constantp (arg &optional env)
@
  return _ecl_funcall3(@'ext::constantp-inner', arg, env);
@)

@(defun ext::constantp-inner (form &optional env)
  cl_object value;
@ {
 AGAIN:
  switch (ecl_t_of(form)) {
  case t_list:
    if (Null(form)) {
      value = ECL_T;
      break;
    }
    if (ECL_CONS_CAR(form) == @'quote') {
      value = ECL_T;
      break;
    }
    /*
      value = cl_macroexpand(2, form, env);
      if (value != form) {
      form = value;
      goto AGAIN;
      }
    */
    value = ECL_NIL;
    break;
  case t_symbol:
    value = cl_macroexpand(2, form, env);
    if (value != form) {
      form = value;
      goto AGAIN;
    }
    if (!(form->symbol.stype & ecl_stp_constant)) {
      value = ECL_NIL;
      break;
    }
  default:
    value = ECL_T;
  }
  ecl_return1(the_env, value);
} @)

@(defun ext::constant-form-value (form &optional env)
  cl_object value;
@ {
 AGAIN:
  switch (ecl_t_of(form)) {
  case t_list:
    if (Null(form)) {
      value = ECL_NIL;
      break;
    }
    if (ECL_CONS_CAR(form) == @'quote') {
      return cl_second(form);
    }
    FEerror("EXT:CONSTANT-FORM-VALUE invoked with a non-constant form ~A",
            0, form);
    break;
  case t_symbol:
    value = cl_macroexpand(2, form, env);
    if (value != form) {
      form = value;
      goto AGAIN;
    }
    value = ECL_SYM_VAL(the_env, value);
    break;
  default:
    value = form;
  }
  @(return value);
} @)


cl_object
ecl_undefined_function_entry(cl_narg narg, ...)
{
  FEundefined_function(ecl_process_env()->function); /* see object.h */
}
