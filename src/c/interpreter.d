/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * interpreter.d - bytecode interpreter
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ecl/ecl-inl.h>
#include <ecl/bytecodes.h>
#include <ecl/internal.h>
#include <ecl/stack-resize.h>

/* -- Errors signaled by the interpreter. ----------------------------------- */

static void
VEbad_lambda_too_many_args(cl_object bytecodes, cl_object frame)
{
  FEprogram_error("Too many arguments passed to "
                  "function ~A~&Argument list: ~S",
                  2, bytecodes, cl_apply(2, @'list', frame));
}

static void
VEbad_lambda_unknown_keyword(cl_object bytecodes, cl_object frame)
{
  FEprogram_error("Unknown keyword argument passed to function ~S.~&"
                  "Argument list: ~S", 2, bytecodes,
                  cl_apply(2, @'list', frame));
}

static void
VEbad_lambda_odd_keys(cl_object bytecodes, cl_object frame)
{
  FEprogram_error("Function ~A called with odd number "
                  "of keyword arguments.",
                  1, bytecodes);
}

static void
VEwrong_arg_type_endp(cl_object reg0)
{
  FEwrong_type_only_arg(@[endp], reg0, @[list]);
}

static void
VEwrong_arg_type_car(cl_object reg0)
{
  FEwrong_type_only_arg(@[car], reg0, @[cons]);
}

static void
VEwrong_arg_type_cdr(cl_object reg0)
{
  FEwrong_type_only_arg(@[cdr], reg0, @[cons]);
}

static void
VEwrong_arg_type_nth_val(cl_fixnum n)
{
  FEerror("Wrong index passed to NTH-VAL", 1, ecl_make_fixnum(n));
}

static void
VEassignment_to_constant(cl_object var)
{
  FEassignment_to_constant(var);
}

static void
VEunbound_variable(cl_object var)
{
  FEunbound_variable(var);
}

static void
VEwrong_num_arguments(cl_object fname)
{
  FEwrong_num_arguments(fname);
}

static void
VEundefined_function(cl_object fun)
{
  FEundefined_function(fun);
}

static void
VEinvalid_function(cl_object fun)
{
  FEinvalid_function(fun);
}

static void
VEclose_around_arg_type()
{
  FEerror("Internal error: ecl_close_around should be called on t_bytecodes or t_bclosure.", 0);
}

/* ------------------------------ LEXICAL ENV. ------------------------------ */
/*
 * A lexical environment is a list of entries, each containing either a variable
 * definition, a tagbody or block tag, or a local function or macro definition.
 *
 *      lex_env ---> ( { record }* )
 *      record = variable | function | block | tagbody | macro | sym_macro
 *
 *      variable =  ( var_name[symbol] . value )
 *      function =  function[bytecodes]
 *      block =     ( tag[fixnum] . block_name[symbol] )
 *      tagbody =   ( tag[fixnum] . 0 )
 *      macro =     ( si::macro macro_function[bytecodes] . macro_name )
 *      sym_macro = ( si::symbol-macro macro_function[bytecodes] . macro_name )
 */

#define bind_lcl(env, entry)      push_lcl(env, entry)

#define bind_var(env, var, val)   bind_lcl(env, CONS(var, val))
#define bind_function(env, fun)   bind_lcl(env, fun)
#define bind_frame(env, id, name) bind_lcl(env, CONS(id, name))

#define unbind_lcl(env, n) drop_lcl(env, n)
#define tangle_lcl(stack) ecl_make_fixnum(stack->frame.sp)
#define unwind_lcl(stack, where) (stack->frame.sp = ecl_fixnum(where))

static void
push_lcl(cl_object stack, cl_object new)
{
  *ECL_STACK_FRAME_TOP(stack) = new;
  stack->frame.sp++;
}

static void
drop_lcl(cl_object stack, cl_fixnum n)
{
  cl_index i;
  for(i=0; i<n; i++) {
    stack->frame.sp--;
    *ECL_STACK_FRAME_TOP(stack) = ECL_NIL;
  }
}

static cl_object
ecl_lcl_env_get_record(cl_object env, cl_fixnum n)
{
  return ECL_STACK_FRAME_REF(env,n);
}

static cl_object
ecl_lex_env_get_record(cl_object env, int s)
{
  return env->vector.self.t[s];
}

#define ecl_lcl_env_get_fun(env,x) ecl_lcl_env_get_record(env,x)
#define ecl_lcl_env_get_blk(env,x) ecl_lcl_env_get_record(env,x)
#define ecl_lcl_env_get_tag(env,x) ecl_lcl_env_get_record(env,x)
#define ecl_lcl_env_get_var(env,x) ECL_CONS_CDR(ecl_lcl_env_get_record(env,x))
#define ecl_lcl_env_set_var(env,x,v) ECL_RPLACD(ecl_lcl_env_get_record(env,x),(v))

#define ecl_lex_env_get_fun(env,x) ecl_lex_env_get_record(env,x)
#define ecl_lex_env_get_blk(env,x) ecl_lex_env_get_record(env,x)
#define ecl_lex_env_get_tag(env,x) ecl_lex_env_get_record(env,x)
#define ecl_lex_env_get_var(env,x) ECL_CONS_CDR(ecl_lex_env_get_record(env,x))
#define ecl_lex_env_set_var(env,x,v) ECL_RPLACD(ecl_lex_env_get_record(env,x),(v))

/* -- Lexical and local env operators ------------------------------------------ */

static cl_object
make_lex(cl_index n)
{
  return si_make_vector(ECL_T, ecl_make_fixnum(n), ECL_NIL,
                        ecl_make_fixnum(0), ECL_NIL, ECL_NIL);
}

static void
push_lex(cl_object stack, cl_object new)
{
  cl_vector_push(new, stack);
}

/* -------------------- AIDS TO THE INTERPRETER -------------------- */

cl_object
_ecl_bytecodes_dispatch_vararg(cl_narg narg, ...)
{
  cl_object output;
  ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame) {
    output = ecl_interpret(frame, ECL_NIL, frame->frame.env->function);
  } ECL_STACK_FRAME_VARARGS_END(frame);
  return output;
}

cl_object
_ecl_bclosure_dispatch_vararg(cl_narg narg, ...)
{
  cl_object output;
  ECL_STACK_FRAME_VARARGS_BEGIN(narg, narg, frame) {
    cl_object fun = frame->frame.env->function;
    output = ecl_interpret(frame, fun->bclosure.lex, fun->bclosure.code);
  } ECL_STACK_FRAME_VARARGS_END(frame);
  return output;
}

/* Find the global function definition associated with a name. This function is
   similar to ecl_fdefinition except thta it does not check for lambdas and
   assumes that the name is either SYMBOL or (SETF SYMBOL). -- jd 2024-12-12 */
static cl_object
_ecl_global_function_definition(cl_object name)
{
  cl_object fun = ECL_NIL, sym, pair;
  switch (ecl_t_of(name)) {
  case t_symbol:
    unlikely_if (!ECL_FBOUNDP(name)
                 || name->symbol.stype & (ecl_stp_macro | ecl_stp_special_form))
      VEundefined_function(name);
    fun = ECL_SYM_FUN(name);
    break;
  case t_list:
    unlikely_if (Null(name))
      VEundefined_function(name);
    /* (setf fname) */
    sym = ECL_CONS_CAR(ECL_CONS_CDR(name));
    pair = sym->symbol.sfdef;
    unlikely_if (Null(pair) || Null(ECL_CONS_CDR(pair))) {
      VEundefined_function(name);
    }
    fun = ECL_CONS_CAR(pair);
    break;
  default:
    VEinvalid_function(name);
  }
  return fun;
}

/* Functions close_around_self and close_around_self_fixup are defined to first
   create a closure (so that it can be bound in locals) and then they modify
   these closures in place to enable self references. -- jd 2025-01-07 */
static cl_object
close_around_self(cl_object fun) {
  cl_object v, template;
  if(ecl_t_of(fun) != t_bytecodes)
    VEclose_around_arg_type();
  template = fun->bytecodes.flex;
  if(Null(template)) return fun;
  /* Make a closure */
  v = ecl_alloc_object(t_bclosure);
  v->bclosure.entry = _ecl_bclosure_dispatch_vararg;
  v->bclosure.code = fun;
  v->bclosure.lex = ECL_NIL;
  return v;
}

static void
close_around_self_fixup(cl_object fun, cl_object lcl_env, cl_object lex_env) {
  cl_object new_lex, template, entry;
  cl_fixnum nlex, idx, ndx;
  switch(ecl_t_of(fun)) {
  case t_bytecodes:
    break;
  case t_bclosure:
    template = fun->bclosure.code->bytecodes.flex;
    /* Close around */
    nlex = template->vector.dim;
    new_lex = make_lex(nlex);
    for (idx = 0; idx<nlex; idx++) {
      entry = template->vector.self.t[idx];
      if(!ECL_FIXNUMP(entry)) {
        push_lex(new_lex, entry);
        continue;
      }
      ndx = ecl_fixnum(template->vector.self.t[idx]);
      ndx < 0
        ? push_lex(new_lex, ecl_lcl_env_get_record(lcl_env, -ndx-1))
        : push_lex(new_lex, ecl_lex_env_get_record(lex_env, ndx));
    }
    /* Fixup the closure */
    fun->bclosure.lex = new_lex;
    break;
  default:
    VEclose_around_arg_type();
  }
}


cl_object
ecl_close_around(cl_object fun, cl_object lcl_env, cl_object lex_env) {
  cl_object v, new_lex, template, entry;
  cl_fixnum nlex, idx, ndx;
  if(ecl_t_of(fun) != t_bytecodes)
    VEclose_around_arg_type();
  template = fun->bytecodes.flex;
  if(Null(template)) return fun;
  /* Close around */
  nlex = template->vector.dim;
  new_lex = make_lex(nlex);
  for (idx = 0; idx<nlex; idx++) {
    entry = template->vector.self.t[idx];
    if(!ECL_FIXNUMP(entry)) {
      push_lex(new_lex, entry);
      continue;
    }
    ndx = ecl_fixnum(template->vector.self.t[idx]);
    ndx < 0
      ? push_lex(new_lex, ecl_lcl_env_get_record(lcl_env, -ndx-1))
      : push_lex(new_lex, ecl_lex_env_get_record(lex_env, ndx));
  }
  /* Make a closure */
  v = ecl_alloc_object(t_bclosure);
  v->bclosure.entry = _ecl_bclosure_dispatch_vararg;
  v->bclosure.code = fun;
  v->bclosure.lex = new_lex;
  /* Profit */
  return v;
}

static inline cl_object
call_stepper(cl_env_ptr the_env, cl_object form, cl_object delta)
{
  return _ecl_funcall3(the_env->stepper, form, delta);
}

#define SETUP_ENV(the_env) { ihs.lex_env = closure; }

/*
 * INTERPRET-FUNCALL is one of the few ways to "exit" the interpreted
 * environment and get into the C/lisp world. Since almost all data
 * from the interpreter is kept in local variables, and frame stacks,
 * binding stacks, etc, are already handled by the C core, only the
 * lexical environment needs to be saved.
 */

#define INTERPRET_FUNCALL(reg0, the_env, frame, narg, fun) {        \
    cl_index __n = narg;                                            \
    cl_index __b = ECL_STACK_INDEX(the_env) - __n;                  \
    SETUP_ENV(the_env);                                             \
    frame.opened = 1;                                               \
    frame.base = __b;                                               \
    frame.size = __n;                                               \
    frame.sp = __b;                                                 \
    reg0 = ecl_apply_from_stack_frame((cl_object)&frame, fun);      \
    ecl_stack_frame_close((cl_object)&frame); }

/* -------------------- THE INTERPRETER -------------------- */

cl_object
ecl_interpret(cl_object frame, cl_object closure, cl_object bytecodes)
{
  ECL_OFFSET_TABLE;
  const cl_env_ptr the_env = frame->frame.env;
  volatile cl_index frame_index = 0;
  cl_opcode *vector = (cl_opcode*)bytecodes->bytecodes.code;
  cl_object lex_env = closure, lcl_env = ECL_NIL;
  cl_object dat_env = bytecodes->bytecodes.data;
  cl_object *data = Null(dat_env) ? NULL : dat_env->vector.self.t;
  cl_object reg0 = ECL_NIL, reg1 = ECL_NIL;
  cl_index narg = 0;
  cl_index nlcl = ecl_fixnum(bytecodes->bytecodes.nlcl);
  struct ecl_stack_frame frame_aux;
  struct ecl_stack_frame frame_lcl;
  volatile struct ecl_ihs_frame ihs;

  /* INV: bytecodes is of type t_bytecodes */
  lcl_env = ecl_cast_ptr(cl_object, &frame_lcl);
  ecl_cs_check(the_env, ihs);
  ecl_ihs_push(the_env, &ihs, bytecodes, closure);
  ecl_stack_frame_open(the_env, lcl_env, nlcl);
  frame_aux.t = t_frame;
  frame_aux.opened = 0;
  frame_aux.base = 0;
  frame_aux.size = 0;
  frame_aux.sp = 0;
  frame_aux.env = the_env;
  BEGIN_SWITCH {
    CASE(OP_NOP); {
      reg0 = ECL_NIL;
      the_env->nvalues = 0;
      THREAD_NEXT;
    }
    /* OP_QUOTE
       Sets REG0 to an immediate value.
    */
    CASE(OP_QUOTE); {
      GET_DATA(reg0, vector, data);
      THREAD_NEXT;
    }
    /* OP_VAR       n{lcl}
       OP_VARC      n{lex}
       OP_VARS      n{dat}
       Sets REG0 to the value of the n-th variable's value.
    */
    CASE(OP_VAR); {
      cl_fixnum ndx;
      GET_OPARG(ndx, vector);
      reg0 = ecl_lcl_env_get_var(lcl_env, ndx);
      THREAD_NEXT;
    }
    CASE(OP_VARC); {
      cl_fixnum ndx;
      GET_OPARG(ndx, vector);
      reg0 = ecl_lex_env_get_var(lex_env, ndx);
      THREAD_NEXT;
    }
    CASE(OP_VARS); {
      cl_object var_name;
      GET_DATA(var_name, vector, data);
      reg0 = ECL_SYM_VAL(the_env, var_name);
      if (ecl_unlikely(reg0 == OBJNULL))
        VEunbound_variable(var_name);
      THREAD_NEXT;
    }

    /* OP_CONS, OP_CAR, OP_CDR, etc
       Inlined forms for some functions which act on reg0 and stack.
    */
    CASE(OP_CONS); {
      cl_object car = ECL_STACK_POP_UNSAFE(the_env);
      reg0 = CONS(car, reg0);
      THREAD_NEXT;
    }

    CASE(OP_CAR); {
      if (ecl_unlikely(!LISTP(reg0)))
        VEwrong_arg_type_car(reg0);
      reg0 = CAR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_CDR); {
      if (ecl_unlikely(!LISTP(reg0)))
        VEwrong_arg_type_cdr(reg0);
      reg0 = CDR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_LIST);
    reg0 = ecl_list1(reg0);

    CASE(OP_LISTA); {
      cl_index n;
      GET_OPARG(n, vector);
      while (--n) {
        reg0 = CONS(ECL_STACK_POP_UNSAFE(the_env), reg0);
      }
      THREAD_NEXT;
    }

    /* OP_CONS_CAR and OP_CONS_CDR
       (Unsafe) primops that act on reg0 and stack.
    */
    CASE(OP_CONS_CAR); {
      reg0 = ECL_CONS_CAR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_CONS_CDR); {
      reg0 = ECL_CONS_CDR(reg0);
      THREAD_NEXT;
    }

    CASE(OP_INT); {
      cl_fixnum n;
      GET_OPARG(n, vector);
      reg0 = ecl_make_fixnum(n);
      THREAD_NEXT;
    }

    CASE(OP_PINT); {
      cl_fixnum n;
      GET_OPARG(n, vector);
      ECL_STACK_PUSH(the_env, ecl_make_fixnum(n));
      THREAD_NEXT;
    }

    /* OP_PUSH
       Pushes the object in REG0.
    */
    CASE(OP_PUSH); {
      ECL_STACK_PUSH(the_env, reg0);
      THREAD_NEXT;
    }
    /* OP_PUSHV     n{lcl}
       OP_PUSHVC    n{lex}
       OP_PUSHVS    n{dat}
       Pushes the value of the n-th variable onto the stack.
    */
    CASE(OP_PUSHV); {
      int ndx;
      GET_OPARG(ndx, vector);
      ECL_STACK_PUSH(the_env, ecl_lcl_env_get_var(lcl_env, ndx));
      THREAD_NEXT;
    }
    CASE(OP_PUSHVC); {
      int ndx;
      GET_OPARG(ndx, vector);
      ECL_STACK_PUSH(the_env, ecl_lex_env_get_var(lex_env, ndx));
      THREAD_NEXT;
    }
    CASE(OP_PUSHVS); {
      cl_object var_name, value;
      GET_DATA(var_name, vector, data);
      value = ECL_SYM_VAL(the_env, var_name);
      if (ecl_unlikely(value == OBJNULL))
        VEunbound_variable(var_name);
      ECL_STACK_PUSH(the_env, value);
      THREAD_NEXT;
    }
    /* OP_PUSHQ     n{arg}
       Pushes n-th constant onto the stack.
    */
    CASE(OP_PUSHQ); {
      cl_object aux;
      GET_DATA(aux, vector, data);
      ECL_STACK_PUSH(the_env, aux);
      THREAD_NEXT;
    }

    CASE(OP_CALLG1); {
      cl_object s;
      cl_objectfn f;
      GET_DATA(s, vector, data);
      the_env->function = ECL_SYM_FUN(s);
      f = ECL_SYM_FUN(s)->cfun.entry;
      SETUP_ENV(the_env);
      reg0 = f(1, reg0);
      THREAD_NEXT;
    }

    CASE(OP_CALLG2); {
      cl_object s;
      cl_objectfn f;
      GET_DATA(s, vector, data);
      the_env->function = ECL_SYM_FUN(s);
      f = ECL_SYM_FUN(s)->cfun.entry;
      SETUP_ENV(the_env);
      reg0 = f(2, ECL_STACK_POP_UNSAFE(the_env), reg0);
      THREAD_NEXT;
    }

    /* OP_CALL      n{arg}
       Calls the function in REG0 with N arguments which
       have been deposited in the stack. The first output value
       is pushed on the stack.
    */
    CASE(OP_CALL); {
      GET_OPARG(narg, vector);
      INTERPRET_FUNCALL(reg0, the_env, frame_aux, narg, reg0);
      THREAD_NEXT;
    }

    /* OP_CALLG     n{arg}, name{arg}
       Calls the function NAME with N arguments which have been
       deposited in the stack. The first output value is pushed on
       the stack.
    */
    CASE(OP_CALLG); {
      GET_OPARG(narg, vector);
      GET_DATA(reg0, vector, data);
      INTERPRET_FUNCALL(reg0, the_env, frame_aux, narg, reg0);
      THREAD_NEXT;
    }

    /* OP_FCALL     n{arg}
       Calls a function in the stack with N arguments which
       have been also deposited in the stack. The output values
       are left in VALUES(...)
    */
    CASE(OP_FCALL); {
      GET_OPARG(narg, vector);
      reg0 = ECL_STACK_REF(the_env,-narg-1);
      INTERPRET_FUNCALL(reg0, the_env, frame_aux, narg, reg0);
      THREAD_NEXT;
    }

    /* OP_MCALL
       Similar to FCALL, but gets the number of arguments from
       the stack (They all have been deposited by OP_PUSHVALUES)
    */
    CASE(OP_MCALL); {
      narg = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      reg0 = ECL_STACK_REF(the_env,-narg-1);
      INTERPRET_FUNCALL(reg0, the_env, frame_aux, narg, reg0);
      THREAD_NEXT;
    }

    /* OP_POP
       Pops a single value pushed by a OP_PUSH* operator.
    */
    CASE(OP_POP); {
      reg0 = ECL_STACK_POP_UNSAFE(the_env);
      THREAD_NEXT;
    }
    /* OP_POP1
       Pops a single value pushed by a OP_PUSH* operator, ignoring it.
    */
    CASE(OP_POP1); {
      (void)ECL_STACK_POP_UNSAFE(the_env);
      THREAD_NEXT;
    }
    /* OP_POPREQ
       Checks the arguments list.
       If there are remaining arguments, REG0 = ARG, otherwise signal an error.
    */
    CASE(OP_POPREQ); {
      if (ecl_unlikely(frame_index >= frame->frame.size)) {
        VEwrong_num_arguments(bytecodes->bytecodes.name);
      }
      reg0 = ECL_STACK_FRAME_REF(frame, frame_index++);
      THREAD_NEXT;
    }
    /* OP_POPOPT
       Checks the arguments list.
       If there are remaining arguments, REG0 = T and the value is on the stack,
       otherwise REG0 = NIL.
    */
    CASE(OP_POPOPT); {
      if (frame_index >= frame->frame.size) {
        reg0 = ECL_NIL;
      } else {
        ECL_STACK_PUSH(the_env, ECL_STACK_FRAME_REF(frame, frame_index++));
        reg0 = ECL_T;
      }
      THREAD_NEXT;
    }
    /* OP_NOMORE
       Asserts that there are no more arguments in the frame.
    */
    CASE(OP_NOMORE); {
      if (ecl_unlikely(frame_index < frame->frame.size))
        VEbad_lambda_too_many_args(bytecodes, frame);
      THREAD_NEXT;
    }
    /* OP_POPREST
       Makes a list out of the remaining arguments.
    */
    CASE(OP_POPREST); {
      cl_object *first = ECL_STACK_FRAME_PTR(frame) + frame_index;
      cl_object *last = ECL_STACK_FRAME_PTR(frame) + frame->frame.size;
      for (reg0 = ECL_NIL; last > first; ) {
        reg0 = CONS(*(--last), reg0);
      }
      THREAD_NEXT;
    }
    /* OP_PUSHKEYS {names-list}
       Checks the stack frame for keyword arguments.
    */
    CASE(OP_PUSHKEYS); {
      cl_object keys_list, aok, *ptr, *end;
      cl_index count, limit;
      GET_DATA(keys_list, vector, data);
      limit = count = frame->frame.size - frame_index;
      if (ecl_unlikely(count & 1)) {
        VEbad_lambda_odd_keys(bytecodes, frame);
      }
      aok = ECL_CONS_CAR(keys_list);
      for (; (keys_list = ECL_CONS_CDR(keys_list), !Null(keys_list)); ) {
        cl_object name = ECL_CONS_CAR(keys_list);
        cl_object flag = ECL_NIL;
        cl_object value = ECL_NIL;
        ptr = ECL_STACK_FRAME_PTR(frame) + frame_index;
        end = ptr + limit;
        for (; ptr != end; ptr++) {
          if (*(ptr++) == name) {
            count -= 2;
            if (flag == ECL_NIL) {
              flag = ECL_T;
              value = *ptr;
            }
          }
        }
        /* Pushing to the stack may resize it, so be careful to reinitialize
           pointers using the new value of ECL_STACK_FRAME_PTR. */
        if (flag != ECL_NIL) ECL_STACK_PUSH(the_env, value);
        ECL_STACK_PUSH(the_env, flag);
      }
      if (count && Null(aok)) {
        ptr = ECL_STACK_FRAME_PTR(frame) + frame_index;
        end = ptr + limit;
        for (; ptr != end; ptr++) {
          if (*(ptr++) == @':allow-other-keys') {
            aok = *ptr;
            count -= 2;
            /* only the first :allow-other-keys argument is considered */
            for (ptr++; ptr != end; ptr++) {
              if (*(ptr++) != @':allow-other-keys')
                break;
              count -= 2;
            }
            break;
          }
        }
        if (ecl_likely(count && Null(aok))) {
          VEbad_lambda_unknown_keyword(bytecodes, frame);
        }
      }
      THREAD_NEXT;
    }
    /* OP_EXIT
       Marks the end of a high level construct (BLOCK, CATCH...)
       or a function.
    */
    CASE(OP_EXIT); {
      ecl_ihs_pop(the_env);
      ecl_stack_frame_close(lcl_env);
      return reg0;
    }
    /* OP_FLET      nfun{arg}, fun1{object}
       ...
       OP_UNBIND nfun
           
       Executes the enclosed code in a lexical enviroment extended with
       the functions "fun1" ... "funn". Note that we only record the
       index of the first function: the others are after this one.
       Note that nfun > 0.
    */
    CASE(OP_FLET); {
      int idx, nfun;
      cl_object fun;
      GET_OPARG(nfun, vector);
      /* Create closures. */
      for(idx = 0; idx<nfun; idx++) {
        GET_DATA(fun, vector, data);
        fun = ecl_close_around(fun, lcl_env, lex_env);
        push_lcl(lcl_env, fun);
      }
      THREAD_NEXT;
    }
    /* OP_LABELS    nfun{arg}
       fun1{object}
       ...
       funn{object}
       ...
       OP_UNBIND n

       Executes the enclosed code in a lexical enviroment extended with
       the functions "fun1" ... "funn".
    */
    CASE(OP_LABELS); {
      cl_index idx, nfun;
      cl_object fun;
      cl_object *sp = ECL_STACK_FRAME_TOP(lcl_env);
      GET_OPARG(nfun, vector);
      /* Create closures. */
      for(idx = 0; idx<nfun; idx++) {
        GET_DATA(fun, vector, data);
        fun = close_around_self(fun);
        push_lcl(lcl_env, fun);
      }
      for(idx = 0; idx<nfun; idx++) {
        fun = *sp++;
        close_around_self_fixup(fun, lcl_env, lex_env);
      }
      THREAD_NEXT;
    }
    /* OP_LFUNCTION index{fixnum} ; local
       OP_CFUNCTION index{fixnum} ; cfb

       Extracts a local function denoted by the index from the environment.
    */
    CASE(OP_LFUNCTION); {
      int ndx;
      GET_OPARG(ndx, vector);
      reg0 = ecl_lcl_env_get_fun(lcl_env, ndx);
      THREAD_NEXT;
    }
    CASE(OP_CFUNCTION); {
      int ndx;
      GET_OPARG(ndx, vector);
      reg0 = ecl_lex_env_get_fun(lex_env, ndx);
      THREAD_NEXT;
    }

    /* OP_FUNCTION  name{function-name}

       Extracts a function associated with the name. The function is defined in
       the global environment. Local function are handled by OP_LFUNCTION and
       lambdas are handled by OP_QUOTE and OP_CLOSE.
    */
    CASE(OP_FUNCTION); {
      GET_DATA(reg0, vector, data);
      reg0 = _ecl_global_function_definition(reg0);
      THREAD_NEXT;
    }

    /* OP_CLOSE fun{object}

       Creates a closure around objects referenced in the current lexical
       environment. Objects may be part of parent locals or its closure.
    */
    CASE(OP_CLOSE); {
      cl_object fun;
      GET_DATA(fun, vector, data);
      reg0 = ecl_close_around(fun, lcl_env, lex_env);
      THREAD_NEXT;
    }

    /* OP_GO        n{arg}, tag-ndx{arg}
       OP_GO_CFB    n{lex}, tag-ndx{arg}

       Jumps to the tag which is defined for the tagbody
       frame registered at the n-th position in the lexical
       environment. TAG-NDX is the number of tag in the list.
    */
    CASE(OP_GO); {
      cl_index ndx;
      cl_fixnum tag_ndx;
      cl_object record;
      GET_OPARG(ndx, vector);
      GET_OPARG(tag_ndx, vector);
      record = ecl_lcl_env_get_tag(lcl_env, ndx);
      /* record = (id . ???) */
      cl_go(ECL_CONS_CAR(record), ecl_make_fixnum(tag_ndx));
      THREAD_NEXT;
    }
    CASE(OP_GO_CFB); {
      cl_fixnum ndx, tag_ndx;
      cl_object record;
      GET_OPARG(ndx, vector);
      GET_OPARG(tag_ndx, vector);
      record = ecl_lex_env_get_tag(lex_env, ndx);
      /* record = (id . ???) */
      cl_go(ECL_CONS_CAR(record), ecl_make_fixnum(tag_ndx));
      THREAD_NEXT;
    }
    /* OP_RETURN       n{arg}
       OP_RETURN_CFB   n{lex}

       Returns from the block whose record in the environment occuppies the n-th
       position.
    */
    CASE(OP_RETURN); {
      int ndx;
      cl_object record;
      GET_OPARG(ndx, vector);
      /* record = (id . name) */
      record = ecl_lcl_env_get_blk(lcl_env, ndx);
      the_env->values[0] = reg0;
      cl_return_from(ECL_CONS_CAR(record), ECL_CONS_CDR(record));
      THREAD_NEXT;
    }
    CASE(OP_RETURN_CFB); {
      int ndx;
      cl_object record;
      GET_OPARG(ndx, vector);
      /* record = (id . name) */
      record = ecl_lex_env_get_blk(lex_env, ndx);
      the_env->values[0] = reg0;
      cl_return_from(ECL_CONS_CAR(record), ECL_CONS_CDR(record));
      THREAD_NEXT;
    }
    /* OP_THROW
       Jumps to an enclosing CATCH form whose tag matches the one
       of the THROW. The tag is taken from the stack, while the
       output values are left in VALUES(...).
    */
    CASE(OP_THROW); {
      cl_object tag_name = ECL_STACK_POP_UNSAFE(the_env);
      the_env->values[0] = reg0;
      cl_throw(tag_name);
      THREAD_NEXT;
    }
    /* OP_JMP       label{arg}
       OP_JNIL      label{arg}
       OP_JT        label{arg}
       OP_JEQ       value{object}, label{arg}
       OP_JNEQ      value{object}, label{arg}
       Direct or conditional jumps. The conditional jumps are made
       comparing with the value of REG0.
    */
    CASE(OP_JMP); {
      cl_oparg jump;
      GET_OPARG(jump, vector);
      vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JNIL); {
      cl_oparg jump;
      GET_OPARG(jump, vector);
      if (Null(reg0))
        vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JT); {
      cl_oparg jump;
      GET_OPARG(jump, vector);
      if (!Null(reg0))
        vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JEQL); {
      cl_oparg value, jump;
      GET_OPARG(value, vector);
      GET_OPARG(jump, vector);
      if (ecl_eql(reg0, data[value]))
        vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }
    CASE(OP_JNEQL); {
      cl_oparg value, jump;
      GET_OPARG(value, vector);
      GET_OPARG(jump, vector);
      if (!ecl_eql(reg0, data[value]))
        vector += jump - OPARG_SIZE;
      THREAD_NEXT;
    }

    CASE(OP_ENDP);
    if (ecl_unlikely(!LISTP(reg0)))
      VEwrong_arg_type_endp(reg0);
    CASE(OP_NOT); {
      reg0 = (reg0 == ECL_NIL)? ECL_T : ECL_NIL;
      THREAD_NEXT;
    }

    /* OP_UNBIND    n{arg}
       Undo "n" local bindings.
    */
    CASE(OP_UNBIND); {
      cl_oparg n;
      GET_OPARG(n, vector);
      unbind_lcl(lcl_env, n);
      THREAD_NEXT;
    }
    /* OP_UNBINDS   n{arg}
       Undo "n" bindings of special variables.
    */
    CASE(OP_UNBINDS); {
      cl_oparg n;
      GET_OPARG(n, vector);
      ecl_bds_unwind_n(the_env, n);
      THREAD_NEXT;
    }
    /* OP_BIND      name{symbol}
       OP_PBIND     name{symbol}
       OP_VBIND     nvalue{arg}, name{symbol}
       OP_BINDS     name{symbol}
       OP_PBINDS    name{symbol}
       OP_VBINDS    nvalue{arg}, name{symbol}
       Binds a lexical or special variable to the the
       value of REG0, the first value of the stack (PBIND) or
       to a given value in the values array.
    */
    CASE(OP_BIND); {
      cl_object var_name;
      GET_DATA(var_name, vector, data);
      bind_var(lcl_env, var_name, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PBIND); {
      cl_object var_name;
      GET_DATA(var_name, vector, data);
      bind_var(lcl_env, var_name, ECL_STACK_POP_UNSAFE(the_env));
      THREAD_NEXT;
    }
    CASE(OP_VBIND); {
      cl_index n;
      cl_object var_name;
      GET_OPARG(n, vector);
      GET_DATA(var_name, vector, data);
      bind_var(lcl_env, var_name,
               (n < the_env->nvalues) ? the_env->values[n] : ECL_NIL);
      THREAD_NEXT;
    }
    CASE(OP_BINDS); {
      cl_object var_name;
      GET_DATA(var_name, vector, data);
      ecl_bds_bind(the_env, var_name, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PBINDS); {
      cl_object var_name;
      GET_DATA(var_name, vector, data);
      ecl_bds_bind(the_env, var_name, ECL_STACK_POP_UNSAFE(the_env));
      THREAD_NEXT;
    }
    CASE(OP_VBINDS); {
      cl_index n;
      cl_object var_name;
      GET_OPARG(n, vector);
      GET_DATA(var_name, vector, data);
      ecl_bds_bind(the_env, var_name,
                   (n < the_env->nvalues) ? the_env->values[n] : ECL_NIL);
      THREAD_NEXT;
    }
    /* OP_SETQ      n{lcl}
       OP_SETQC     n{lex}
       OP_SETQS     n{dat}

       OP_PSETQ     n{lcl}
       OP_PSETQC    n{lex}
       OP_PSETQS    n{dat}

       OP_VSETQ     n{lcl}, nvalue{arg}
       OP_VSETQC    n{lex}, nvalue{arg}
       OP_VSETQS    n{dat}, nvalue{arg}

       Sets either the n-th variable to either the value in REG0 (OP_SETQ[CS])
       or to the first value on the stack (OP_PSETQ[CS]), or to a given value
       from the multiple values array (OP_VSETQ[CS]). Note NVALUE > 0 strictly.
    */
    CASE(OP_SETQ); {
      int ndx;
      GET_OPARG(ndx, vector);
      ecl_lcl_env_set_var(lcl_env, ndx, reg0);
      THREAD_NEXT;
    }
    CASE(OP_SETQC); {
      int ndx;
      GET_OPARG(ndx, vector);
      ecl_lex_env_set_var(lex_env, ndx, reg0);
      THREAD_NEXT;
    }
    CASE(OP_SETQS); {
      cl_object var;
      GET_DATA(var, vector, data);
      /* INV: Not NIL, and of type t_symbol */
      if (ecl_unlikely(var->symbol.stype & ecl_stp_constant))
        VEassignment_to_constant(var);
      ECL_SETQ(the_env, var, reg0);
      THREAD_NEXT;
    }
    CASE(OP_PSETQ); {
      int ndx;
      GET_OPARG(ndx, vector);
      ecl_lcl_env_set_var(lcl_env, ndx, ECL_STACK_POP_UNSAFE(the_env));
      THREAD_NEXT;
    }
    CASE(OP_PSETQC); {
      int ndx;
      GET_OPARG(ndx, vector);
      ecl_lex_env_set_var(lex_env, ndx, ECL_STACK_POP_UNSAFE(the_env));
      THREAD_NEXT;
    }
    CASE(OP_PSETQS); {
      cl_object var;
      GET_DATA(var, vector, data);
      /* INV: Not NIL, and of type t_symbol */
      ECL_SETQ(the_env, var, ECL_STACK_POP_UNSAFE(the_env));
      THREAD_NEXT;
    }
    CASE(OP_VSETQ); {
      cl_index ndx;
      cl_oparg index;
      cl_object value;
      GET_OPARG(ndx, vector);
      GET_OPARG(index, vector);
      value = (index >= the_env->nvalues)
        ? ECL_NIL
        : the_env->values[index];
      ecl_lcl_env_set_var(lcl_env, ndx, value);
      THREAD_NEXT;
    }
    CASE(OP_VSETQC); {
      cl_index ndx;
      cl_oparg index;
      cl_object value;
      GET_OPARG(ndx, vector);
      GET_OPARG(index, vector);
      value = (index >= the_env->nvalues)
        ? ECL_NIL
        : the_env->values[index];
      ecl_lex_env_set_var(lex_env, ndx, value);
      THREAD_NEXT;
    }
    CASE(OP_VSETQS); {
      cl_object var, v;
      cl_oparg index;
      GET_DATA(var, vector, data);
      GET_OPARG(index, vector);
      v = (index >= the_env->nvalues)? ECL_NIL : the_env->values[index];
      ECL_SETQ(the_env, var, v);
      THREAD_NEXT;
    }
                        
    /* OP_BLOCK     constant
       OP_DO
       OP_CATCH

       OP_FRAME     label{arg}
       ...
       OP_EXIT_FRAME
       label:
    */

    CASE(OP_BLOCK); {
      GET_DATA(reg0, vector, data);
      reg1 = ecl_make_fixnum(the_env->frs_stack.frame_id++);
      bind_frame(lcl_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_DO); {
      reg0 = ECL_NIL;
      reg1 = ecl_make_fixnum(the_env->frs_stack.frame_id++);
      bind_frame(lcl_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_CATCH); {
      reg1 = reg0;
      bind_frame(lcl_env, reg1, reg0);
      THREAD_NEXT;
    }
    CASE(OP_FRAME); {
      cl_opcode *exit;
      GET_LABEL(exit, vector);
      ECL_STACK_PUSH(the_env, tangle_lcl(lcl_env));
      ECL_STACK_PUSH(the_env, (cl_object)exit);
      ecl_frs_push(the_env,reg1);
      if (__ecl_frs_push_result != 0) {
        reg0 = the_env->values[0];
        vector = (cl_opcode *)ECL_STACK_REF(the_env,-1); /* FIXME! */
        /* Unbind locals including the frame, we are leaving the frame. */
        unwind_lcl(lcl_env, ECL_STACK_REF(the_env, -2));
        unbind_lcl(lcl_env, 1); /* unbind the frame */
        goto DO_EXIT_FRAME;
      }
      THREAD_NEXT;
    }
    /* OP_FRAMEID   0
       OP_TAGBODY   n{arg}
       label1
       ...
       labeln
       label1:
       ...
       labeln:
       ...
       OP_EXIT_TAGBODY

       High level construct for the TAGBODY form.
    */
    CASE(OP_TAGBODY); {
      int n;
      GET_OPARG(n, vector);
      ECL_STACK_PUSH(the_env, tangle_lcl(lcl_env));
      ECL_STACK_PUSH(the_env, (cl_object)vector); /* FIXME! */
      vector += n * OPARG_SIZE;
      ecl_frs_push(the_env,reg1);
      if (__ecl_frs_push_result != 0) {
        /* Wait here for gotos. Each goto sets VALUES(0) to an integer which
           ranges from 0 to ntags-1, depending on the tag. These numbers are
           indices into the jump table and are computed at compile time. */
        cl_opcode *table = (cl_opcode *)ECL_STACK_REF(the_env,-1);
        /* Unbind locals but leave the frame, we are still inside the frame. */
        unwind_lcl(lcl_env, ECL_STACK_REF(the_env,-2));
        table = table + ecl_fixnum(the_env->values[0]) * OPARG_SIZE;
        vector = table + *(cl_oparg *)table;
      }
      THREAD_NEXT;
    }
    CASE(OP_EXIT_TAGBODY); {
      reg0 = ECL_NIL;
    }
    CASE(OP_EXIT_FRAME); {
    DO_EXIT_FRAME:
      ecl_frs_pop(the_env);
      ECL_STACK_POP_N_UNSAFE(the_env, 2);
      THREAD_NEXT;
    }
    CASE(OP_NIL); {
      reg0 = ECL_NIL;
      THREAD_NEXT;
    }
    CASE(OP_PUSHNIL); {
      ECL_STACK_PUSH(the_env, ECL_NIL);
      THREAD_NEXT;
    }
    CASE(OP_VALUEREG0); {
      the_env->nvalues = 1;
      THREAD_NEXT;
    }

    /* OP_PUSHVALUES
       Pushes the values output by the last form, plus the number
       of values.
    */
  PUSH_VALUES:
    CASE(OP_PUSHVALUES); {
      cl_index i = the_env->nvalues;
      ECL_STACK_PUSH_N(the_env, i+1);
      the_env->values[0] = reg0;
      memcpy(&ECL_STACK_REF(the_env, -(i+1)), the_env->values, i * sizeof(cl_object));
      ECL_STACK_REF(the_env, -1) = ecl_make_fixnum(the_env->nvalues);
      THREAD_NEXT;
    }
    /* OP_PUSHMOREVALUES
       Adds more values to the ones pushed by OP_PUSHVALUES.
    */
    CASE(OP_PUSHMOREVALUES); {
      cl_index n = ecl_fixnum(ECL_STACK_REF(the_env,-1));
      cl_index i = the_env->nvalues;
      ECL_STACK_PUSH_N(the_env, i);
      the_env->values[0] = reg0;
      memcpy(&ECL_STACK_REF(the_env, -(i+1)), the_env->values, i * sizeof(cl_object));
      ECL_STACK_REF(the_env, -1) = ecl_make_fixnum(n + i);
      THREAD_NEXT;
    }
    /* OP_POPVALUES
       Pops all values pushed by a OP_PUSHVALUES operator.
    */
    CASE(OP_POPVALUES); {
      cl_object *dest = the_env->values;
      int n = the_env->nvalues = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      if (n == 0) {
        *dest = reg0 = ECL_NIL;
        THREAD_NEXT;
      } else if (n == 1) {
        *dest = reg0 = ECL_STACK_POP_UNSAFE(the_env);
        THREAD_NEXT;
      } else {
        ECL_STACK_POP_N_UNSAFE(the_env,n);
        memcpy(dest, &ECL_STACK_REF(the_env,0), n * sizeof(cl_object));
        reg0 = *dest;
        THREAD_NEXT;
      }
    }
    /* OP_VALUES    n{arg}
       Pop N values from the stack and store them in VALUES(...)
       Note that N is strictly > 0.
    */
    CASE(OP_VALUES); {
      cl_fixnum n;
      GET_OPARG(n, vector);
      the_env->nvalues = n;
      ECL_STACK_POP_N_UNSAFE(the_env, n);
      memcpy(the_env->values, &ECL_STACK_REF(the_env, 0), n * sizeof(cl_object));
      reg0 = the_env->values[0];
      THREAD_NEXT;
    }
    /* OP_NTHVAL
       Set VALUES(0) to the N-th value of the VALUES(...) list.
       The index N-th is extracted from the top of the stack.
    */
    CASE(OP_NTHVAL); {
      cl_fixnum n = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      if (ecl_unlikely(n < 0)) {
        VEwrong_arg_type_nth_val(n);
      } else if ((cl_index)n >= the_env->nvalues) {
        reg0 = ECL_NIL;
      } else if (n) {
        reg0 = the_env->values[n];
      }
      THREAD_NEXT;
    }
    /* OP_PROTECT   label
       ...        ; code to be protected and whose value is output
       OP_PROTECT_NORMAL
       label:
       ...        ; code executed at exit
       OP_PROTECT_EXIT

       High level construct for UNWIND-PROTECT. The first piece of code is
       executed and its output value is saved. Then the second piece of code
       is executed and the output values restored. The second piece of code
       is always executed, even if a THROW, RETURN or GO happen within the
       first piece of code.
    */
    CASE(OP_PROTECT); {
      cl_opcode *exit;
      GET_LABEL(exit, vector);
      ECL_STACK_PUSH(the_env, tangle_lcl(lcl_env));
      ECL_STACK_PUSH(the_env, (cl_object)exit);
      ecl_frs_push(the_env,ECL_PROTECT_TAG);
      if (__ecl_frs_push_result != 0) {
        ecl_frs_pop(the_env);
        vector = (cl_opcode *)ECL_STACK_POP_UNSAFE(the_env);
        unwind_lcl(lcl_env, ECL_STACK_POP_UNSAFE(the_env));
        reg0 = the_env->values[0];
        ECL_STACK_PUSH(the_env, ecl_make_fixnum(the_env->frs_stack.nlj_fr - the_env->frs_stack.top));
        goto PUSH_VALUES;
      }
      THREAD_NEXT;
    }
    CASE(OP_PROTECT_NORMAL); {
      ecl_bds_unwind(the_env, the_env->frs_stack.top->frs_bds_ndx);
      ecl_frs_pop(the_env);
      (void)ECL_STACK_POP_UNSAFE(the_env);
      unwind_lcl(lcl_env, ECL_STACK_POP_UNSAFE(the_env));
      ECL_STACK_PUSH(the_env, ecl_make_fixnum(1));
      goto PUSH_VALUES;
    }
    CASE(OP_PROTECT_EXIT); {
      volatile cl_fixnum n = the_env->nvalues = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      while (n--)
        the_env->values[n] = ECL_STACK_POP_UNSAFE(the_env);
      reg0 = the_env->values[0];
      n = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      if (n <= 0)
        ecl_unwind(the_env, the_env->frs_stack.top + n);
      THREAD_NEXT;
    }

    /* OP_PROGV     bindings{list}
       ...
       OP_EXIT
       Execute the code enclosed with the special variables in BINDINGS
       set to the values in the list which was passed in VALUES(0).
    */
    CASE(OP_PROGV); {
      cl_object values = reg0;
      cl_object vars = ECL_STACK_POP_UNSAFE(the_env);
      cl_index n = ecl_progv(the_env, vars, values);
      ECL_STACK_PUSH(the_env, ecl_make_fixnum(n));
      THREAD_NEXT;
    }
    CASE(OP_EXIT_PROGV); {
      cl_index n = ecl_fixnum(ECL_STACK_POP_UNSAFE(the_env));
      ecl_bds_unwind(the_env, n);
      THREAD_NEXT;
    }
    CASE(OP_CSET); {
      cl_object *p;
      GET_DATA_PTR(p, vector, data);
      *p = reg0;
      THREAD_NEXT;
    }

    CASE(OP_STEPIN); {
      cl_object form;
      cl_index n;
      GET_DATA(form, vector, data);
      SETUP_ENV(the_env);
      the_env->values[0] = reg0;
      n = ecl_data_stack_push_values(the_env);
      call_stepper(the_env, form, ecl_make_fixnum(1));
      ecl_data_stack_pop_values(the_env, n);
      reg0 = the_env->values[0];
      THREAD_NEXT;
    }
    CASE(OP_STEPCALL); {
      /* We are going to call a function. However, we would
       * like to step _in_ the function. STEPPER takes care of
       * that. */
      cl_fixnum n;
      GET_OPARG(n, vector);
      SETUP_ENV(the_env);
      reg0 = call_stepper(the_env, reg0, ecl_make_fixnum(0));
      INTERPRET_FUNCALL(reg0, the_env, frame_aux, n, reg0);
    }
    CASE(OP_STEPOUT); {
      cl_index n;
      SETUP_ENV(the_env);
      the_env->values[0] = reg0;
      n = ecl_data_stack_push_values(the_env);
      call_stepper(the_env, ECL_NIL, ecl_make_fixnum(-1));
      ecl_data_stack_pop_values(the_env, n);
      reg0 = the_env->values[0];
      THREAD_NEXT;
    }
  }
}

cl_object
si_interpreter_stack ()
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ECL_NIL);
}
