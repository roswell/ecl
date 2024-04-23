/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.d - ECL stacks
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <signal.h>
#include <string.h>
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif
#include <ecl/nucleus.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/stack-resize.h>

/* -- General purpose LISP stack --------------------------------------------- **

This structure is most notably used by the VMS stack. When we make environments
cl_object instances then other stacks will use this code too.

FIXME / TODO:
- add operators CREATE and RESIZE
- add restarts for overflow and better errors
- add more operators (c.f. forth)
- implement extensible sequences protocol
- add stack frames as first-class citizen

** ----------------------------------------------------------------------------*/

cl_object
si_stack_push(cl_object self, cl_object elt)
{
  cl_env_ptr the_env = ecl_process_env();
  if (self->stack.top >= self->stack.limit) {
    FEerror("Stack overflow.",0);
  }
  ecl_return1(the_env, ecl_stack_push(self, elt));
}

cl_object
si_stack_pop(cl_object self)
{
  cl_env_ptr the_env = ecl_process_env();
  if (self->stack.top <= self->stack.org) {
    FEerror("Stack underflow.",0);
  }
  ecl_return1(the_env, ecl_stack_popu(self));
}

cl_object
si_stack_top(cl_object self)
{
  cl_env_ptr the_env = ecl_process_env();
  if (self->stack.top <= self->stack.org) {
    FEerror("Stack is empty.",0);
  }
  ecl_return1(the_env, ECL_STACK_TOP(self));
}

/* ------------------------- C STACK ---------------------------------- */

static void
cs_set_size(cl_env_ptr env, cl_index new_size);

void
ecl_cs_init(cl_env_ptr env)
{
#ifdef GBC_BOEHM
  struct GC_stack_base base;
  if (GC_get_stack_base(&base) == GC_SUCCESS)
    env->c_stack.org = (char*)base.mem_base;
  else
#endif
    {
      /* Rough estimate. Not very safe. We assume that cl_boot()
       * is invoked from the main() routine of the program. */
      env->c_stack.org = (char*)(&env);
    }
  env->c_stack.max = env->c_stack.org;
  env->c_stack.max_size = 0;
  cs_set_size(env, ecl_option_values[ECL_OPT_C_STACK_SIZE]);
}

static void
cs_set_size(cl_env_ptr env, cl_index new_size)
{
  volatile char foo = 0;
  cl_index margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
  if (new_size > env->c_stack.max_size)
    new_size = env->c_stack.max_size;
#if defined(ECL_CAN_SET_STACK_SIZE)
  {
    struct rlimit rl;

    if (!getrlimit(RLIMIT_STACK, &rl)) {
      env->c_stack.max_size = rl.rlim_max;
      if (new_size > rl.rlim_cur) {
        rl.rlim_cur = (new_size > rl.rlim_max) ? rl.rlim_max : new_size;
        if (setrlimit(RLIMIT_STACK, &rl))
          ecl_internal_error("Can't set the size of the C stack");
      }
    } else {
      rl.rlim_cur = new_size;
    }
    if (rl.rlim_cur == 0 || rl.rlim_cur == RLIM_INFINITY || rl.rlim_cur > (cl_index)(-1)) {
      /* Either getrlimit failed or returned nonsense, either way we
       * don't know the stack size. Use a default of 1 MB and hope for
       * the best. */
      new_size = 1048576;
    } else {
      new_size = rl.rlim_cur;
    }
#ifdef ECL_DOWN_STACK
    env->c_stack.max = env->c_stack.org - new_size;
#else
    env->c_stack.max = env->c_stack.org + new_size;
#endif
  }
#endif
  env->c_stack.limit_size = new_size - (2*margin);
#ifdef ECL_DOWN_STACK
  if (&foo > (env->c_stack.org - new_size) + 16) {
    env->c_stack.limit = (env->c_stack.org - new_size) + (2*margin);
    if (env->c_stack.limit < env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  }
#else
  if (&foo < (env->c_stack.org + new_size) - 16) {
    env->c_stack.limit = (env->c_stack.org + new_size) - (2*margin);
    if (env->c_stack.limit > env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  }
#endif
  else
    ecl_internal_error("Can't set the size of the C stack: sanity check failed");
  env->c_stack.size = new_size;
}

void
ecl_cs_overflow(void)
{
  static const char *stack_overflow_msg =
    "\n;;;\n;;; Stack overflow.\n"
    ";;; Jumping to the outermost toplevel prompt\n"
    ";;;\n\n";
  cl_env_ptr env = ecl_process_env();
  cl_index margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
  cl_index size = env->c_stack.size;
#ifdef ECL_DOWN_STACK
  if (env->c_stack.limit > env->c_stack.org - size)
    env->c_stack.limit -= margin;
#else
  if (env->c_stack.limit < env->c_stack.org + size)
    env->c_stack.limit += margin;
#endif
  else
    ecl_internal_error(stack_overflow_msg);
  if (env->c_stack.max_size == (cl_index)0 || env->c_stack.size < env->c_stack.max_size)
    ecl_cerror(ECL_EX_CS_OVR, ecl_make_fixnum(size), ECL_YES);
  else
    ecl_ferror(ECL_EX_CS_OVR, ecl_make_fixnum(size), ECL_NIL);
}

/* ------------------------- LISP STACK ------------------------------- */

static void
vms_init(cl_env_ptr env)
{
  cl_index size, margin, limit_size;
  margin = ecl_option_values[ECL_OPT_LISP_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_LISP_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->vms_stack.t = t_stack;
  env->vms_stack.org = (cl_object *)ecl_malloc(size * sizeof(cl_object));
  env->vms_stack.top = env->vms_stack.org;
  env->vms_stack.limit = &env->vms_stack.org[limit_size];
  env->vms_stack.size = size;
  env->vms_stack.limit_size = limit_size;
  /* A stack always has at least one element. This is assumed by cl__va_start
     and friends, which take a sp=0 to have no arguments. */
  *(env->vms_stack.top++) = ecl_make_fixnum(0);
}

void
vms_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_LISP_STACK_SAFETY_AREA];
  cl_object *old_org = env->vms_stack.org;
  cl_object *new_org = NULL;
  cl_index new_max_size = new_lim_size + 2*margin;
  cl_index current_size = env->vms_stack.top - old_org;
  if (current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  new_org = ecl_realloc(old_org, new_max_size * sizeof(*old_org));
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  env->vms_stack.org = new_org;
  env->vms_stack.top = new_org + current_size;
  env->vms_stack.limit = new_org + new_lim_size;
  /* Update indexes */
  env->vms_stack.size = new_max_size;
  env->vms_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

static cl_object *
vms_grow(cl_env_ptr env)
{
  vms_set_limit(env, env->vms_stack.limit_size + env->vms_stack.limit_size / 2);
  return env->vms_stack.top;
}

cl_index
ecl_vms_push_values(cl_env_ptr env) {
  cl_index i = env->nvalues;
  cl_object *b = env->vms_stack.top;
  cl_object *p = b + i;
  if (p >= env->vms_stack.limit) {
    b = vms_grow(env);
    p = b + i;
  }
  env->vms_stack.top = p;
  ecl_copy(b, env->values, i * sizeof(cl_object));
  return i;
}

void
ecl_vms_pop_values(cl_env_ptr env, cl_index n) {
  cl_object *p = env->vms_stack.top - n;
  if (ecl_unlikely(p < env->vms_stack.org))
    ecl_internal_error("vms: stack underflow.");
  env->nvalues = n;
  env->vms_stack.top = p;
  ecl_copy(env->values, p, n * sizeof(cl_object));
}

cl_object
ecl_stack_frame_open(cl_env_ptr env, cl_object f, cl_index size)
{
  cl_object *base = env->vms_stack.top;
  if (size) {
    if ((env->vms_stack.limit - base) < size) {
      vms_set_limit(env, env->vms_stack.limit_size + size);
      base = env->vms_stack.top;
    }
  }
  f->frame.t = t_frame;
  f->frame.stack = env->vms_stack.org;
  f->frame.base = base;
  f->frame.size = size;
  f->frame.env = env;
  env->vms_stack.top = (base + size);
  return f;
}

void
ecl_stack_frame_push(cl_object f, cl_object o)
{
  cl_env_ptr env = f->frame.env;
  cl_object *top = env->vms_stack.top;
  if (top >= env->vms_stack.limit) {
    top = vms_grow(env);
  }
  env->vms_stack.top = ++top;
  *(top-1) = o;
  f->frame.base = top - (++(f->frame.size));
  f->frame.stack = env->vms_stack.org;
}

void
ecl_stack_frame_push_values(cl_object f)
{
  cl_env_ptr env = f->frame.env;
  ecl_vms_push_values(env);
  f->frame.base = env->vms_stack.top - (f->frame.size += env->nvalues);
  f->frame.stack = env->vms_stack.org;
}

cl_object
ecl_stack_frame_pop_values(cl_object f)
{
  cl_env_ptr env = f->frame.env;
  cl_index n = f->frame.size % ECL_MULTIPLE_VALUES_LIMIT;
  cl_object o;
  env->nvalues = n;
  env->values[0] = o = ECL_NIL;
  while (n--) {
    env->values[n] = o = f->frame.base[n];
  }
  return o;
}

void
ecl_stack_frame_close(cl_object f)
{
  if (f->frame.stack) {
    cl_env_ptr the_env = f->frame.env;
    cl_object vms = ecl_cast_ptr(cl_object,&the_env->vms_stack);
    ecl_stack_unwind(vms, f->frame.base - f->frame.stack);
  }
}

/* ------------------------- BINDING STACK ---------------------------- */

static void
bds_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  ecl_bds_ptr old_org = env->bds_stack.org;
  ecl_bds_ptr new_org = NULL;
  cl_index new_max_size = new_lim_size + 2*margin;
  cl_index current_size = env->bds_stack.top - old_org;
  if (current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  new_org = ecl_realloc(old_org, new_max_size * sizeof(*old_org));
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  env->bds_stack.org = new_org;
  env->bds_stack.top = new_org + current_size;
  env->bds_stack.limit = new_org + new_lim_size;
  /* Update indexes */
  env->bds_stack.size = new_max_size;
  env->bds_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

cl_index
ecl_progv(cl_env_ptr env, cl_object vars0, cl_object values0)
{
  cl_object vars = vars0, values = values0;
  cl_index n = env->bds_stack.top - env->bds_stack.org;
  for (; LISTP(vars) && LISTP(values); vars = ECL_CONS_CDR(vars)) {
    if (Null(vars)) {
      return n;
    } else {
      cl_object var = ECL_CONS_CAR(vars);
      if (!ECL_SYMBOLP(var))
        FEillegal_variable_name(var);
      if (ecl_symbol_type(var) & ecl_stp_constant)
        FEbinding_a_constant(var);
      if (Null(values)) {
        ecl_bds_bind(env, var, OBJNULL);
      } else {
        ecl_bds_bind(env, var, ECL_CONS_CAR(values));
        values = ECL_CONS_CDR(values);
      }
    }
  }
  FEerror("Wrong arguments to special form PROGV. Either~%"
          "~A~%or~%~A~%are not proper lists",
          2, vars0, values0);
}

static ecl_bds_ptr
get_bds_ptr(cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    cl_env_ptr env = ecl_process_env();
    ecl_bds_ptr p = env->bds_stack.org + ecl_fixnum(x);
    if (env->bds_stack.org <= p && p <= env->bds_stack.top)
      return(p);
  }
  FEerror("~S is an illegal bds index.", 1, x);
}

cl_object
si_bds_top()
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->bds_stack.top - env->bds_stack.org));
}

cl_object
si_bds_var(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_bds_ptr(arg)->symbol);
}

cl_object
si_bds_val(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  cl_object v = get_bds_ptr(arg)->value;
  ecl_return1(env, ((v == OBJNULL || v == ECL_NO_TL_BINDING)? ECL_UNBOUND : v));
}

/* ------------------------- INVOCATION STACK ------------------------- */

static void
ihs_init(cl_env_ptr env)
{
  static struct ecl_ihs_frame ihs_org = { NULL, NULL, NULL, 0};
  env->ihs_stack.top = &ihs_org;
  ihs_org.function = ECL_NIL;
  ihs_org.lex_env = ECL_NIL;
  ihs_org.index = 0;
}

static ecl_ihs_ptr
get_ihs_ptr(cl_index n)
{
  cl_env_ptr env = ecl_process_env();
  ecl_ihs_ptr p = env->ihs_stack.top;
  if (n > p->index)
    FEerror("~D is an illegal IHS index.", 1, ecl_make_fixnum(n));
  while (n < p->index)
    p = p->next;
  return p;
}

cl_object
si_ihs_top(void)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->ihs_stack.top->index));
}

cl_object
si_ihs_prev(cl_object x)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, cl_1M(x));
}

cl_object
si_ihs_next(cl_object x)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, cl_1P(x));
}

cl_object
si_ihs_bds(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_ihs_ptr(ecl_to_size(arg))->bds));
}

cl_object
si_ihs_fun(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->function);
}

cl_object
si_ihs_env(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_ihs_ptr(ecl_to_size(arg))->lex_env);
}

/* ------------------------- FRAME STACK ------------------------------ */

static void
frs_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  ecl_frame_ptr old_org = env->frs_stack.org;
  ecl_frame_ptr new_org = NULL;
  cl_index new_max_size = new_lim_size + 2*margin;
  cl_index current_size = env->frs_stack.top - old_org;
  if(current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  new_org = ecl_realloc(old_org, new_max_size * sizeof(*old_org));
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  env->frs_stack.org = new_org;
  env->frs_stack.top = new_org + current_size;
  env->frs_stack.limit = new_org + new_lim_size;
  /* Update indexes. */
  env->frs_stack.size = new_max_size;
  env->frs_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

static ecl_frame_ptr
get_frame_ptr(cl_object x)
{
  if (ECL_FIXNUMP(x)) {
    cl_env_ptr env = ecl_process_env();
    ecl_frame_ptr p = env->frs_stack.org + ecl_fixnum(x);
    if (env->frs_stack.org <= p && p <= env->frs_stack.top)
      return p;
  }
  FEerror("~S is an illegal frs index.", 1, x);
}

cl_object
si_frs_top()
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(env->frs_stack.top - env->frs_stack.org));
}

cl_object
si_frs_bds(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_frame_ptr(arg)->frs_bds_ndx));
}

cl_object
si_frs_tag(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, get_frame_ptr(arg)->frs_val);
}

cl_object
si_frs_ihs(cl_object arg)
{
  cl_env_ptr env = ecl_process_env();
  ecl_return1(env, ecl_make_fixnum(get_frame_ptr(arg)->frs_ihs->index));
}

cl_object
si_sch_frs_base(cl_object fr, cl_object ihs)
{
  cl_env_ptr env = ecl_process_env();
  ecl_frame_ptr x;
  cl_index y = ecl_to_size(ihs);
  for (x = get_frame_ptr(fr); 
       x <= env->frs_stack.top && x->frs_ihs->index < y;
       x++);
  ecl_return1(env, ((x > env->frs_stack.top)
                    ? ECL_NIL
                    : ecl_make_fixnum(x - env->frs_stack.org)));
}

/* ------------------------- INITIALIZATION --------------------------- */

cl_object
si_set_limit(cl_object type, cl_object limit)
{
  cl_env_ptr env = ecl_process_env();
  cl_index margin;
  if (type == @'ext::frame-stack') {
    cl_index current_size = env->frs_stack.top - env->frs_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink FRS stack below ~D.", 1, limit);
    frs_set_limit(env, request_size);
  } else if (type == @'ext::binding-stack') {
    cl_index current_size = env->bds_stack.top - env->bds_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink BDS stack below ~D.", 1, limit);
    bds_set_limit(env, request_size);
  } else if (type == @'ext::lisp-stack') {
    cl_index current_size = env->vms_stack.top - env->vms_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink VMS stack below ~D.", 1, limit);
    vms_set_limit(env, request_size);
  } else if (type == @'ext::c-stack') {
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
    cs_set_size(env, the_size + 2*margin);
  } else if (type == @'ext::heap-size') {
    /*
     * size_t can be larger than cl_index, and ecl_to_size()
     * creates a fixnum which is too small for size_t on 32-bit.
     */
    size_t the_size = (size_t)ecl_to_ulong(limit);
    _ecl_set_max_heap_size(the_size);
  }

  ecl_return1(env, si_get_limit(type));
}

cl_object
si_get_limit(cl_object type)
{
  cl_env_ptr env = ecl_process_env();
  cl_index output = 0;
  if (type == @'ext::frame-stack')
    output = env->frs_stack.limit_size;
  else if (type == @'ext::binding-stack')
    output = env->bds_stack.limit_size;
  else if (type == @'ext::lisp-stack')
    output = env->vms_stack.limit_size;
  else if (type == @'ext::c-stack')
    output = env->c_stack.limit_size;
  else if (type == @'ext::heap-size') {
    /* size_t can be larger than cl_index */
    ecl_return1(env, ecl_make_unsigned_integer(ecl_core.max_heap_size));
  }

  ecl_return1(env, ecl_make_unsigned_integer(output));
}

void
init_stacks(cl_env_ptr env)
{
  init_early_stacks(env);
  ihs_init(env);
  vms_init(env);
}
