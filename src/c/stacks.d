/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.d - binding/history/frame stacks
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
#include <ecl/internal.h>
#include <ecl/stack-resize.h>

/* ------------------------- C STACK ---------------------------------- */

static void
cs_set_size(cl_env_ptr env, cl_index new_size)
{
  volatile char foo = 0;
  cl_index margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
#if defined(ECL_CAN_SET_STACK_SIZE)
  {
    struct rlimit rl;

    if (!getrlimit(RLIMIT_STACK, &rl)) {
      env->cs_max_size = rl.rlim_max;
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
    env->cs_barrier = env->cs_org - new_size;
#else
    env->cs_barrier = env->cs_org + new_size;
#endif
  }
#endif
  env->cs_limit_size = new_size - (2*margin);
#ifdef ECL_DOWN_STACK
  if (&foo > (env->cs_org - new_size) + 16) {
    env->cs_limit = (env->cs_org - new_size) + (2*margin);
    if (env->cs_limit < env->cs_barrier)
      env->cs_barrier = env->cs_limit;
  }
#else
  if (&foo < (env->cs_org + new_size) - 16) {
    env->cs_limit = (env->cs_org + new_size) - (2*margin);
    if (env->cs_limit > env->cs_barrier)
      env->cs_barrier = env->cs_limit;
  }
#endif
  else
    ecl_internal_error("Can't set the size of the C stack: sanity check failed");
  env->cs_size = new_size;
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
  cl_index size = env->cs_size;
#ifdef ECL_DOWN_STACK
  if (env->cs_limit > env->cs_org - size)
    env->cs_limit -= margin;
#else
  if (env->cs_limit < env->cs_org + size)
    env->cs_limit += margin;
#endif
  else
    ecl_unrecoverable_error(env, stack_overflow_msg);

  if (env->cs_max_size == (cl_index)0 || env->cs_size < env->cs_max_size)
    si_serror(6, @"Extend stack size",
              @'ext::stack-overflow',
              @':size', ecl_make_fixnum(size),
              @':type', @'ext::c-stack');
  else
    si_serror(6, ECL_NIL,
              @'ext::stack-overflow',
              @':size', ECL_NIL,
              @':type', @'ext::c-stack');
  size += size/2;
  if (size > env->cs_max_size)
    size = env->cs_max_size;
  cs_set_size(env, size);
}

void
ecl_cs_set_org(cl_env_ptr env)
{
#ifdef GBC_BOEHM
  struct GC_stack_base base;
  if (GC_get_stack_base(&base) == GC_SUCCESS)
    env->cs_org = (char*)base.mem_base;
  else
#endif
    {
      /* Rough estimate. Not very safe. We assume that cl_boot()
       * is invoked from the main() routine of the program.
       */
      env->cs_org = (char*)(&env);
    }
  env->cs_barrier = env->cs_org;
  env->cs_max_size = 0;
  cs_set_size(env, ecl_option_values[ECL_OPT_C_STACK_SIZE]);
}

/* ------------------------- LISP STACK ------------------------------- */

cl_object *
ecl_stack_set_size(cl_env_ptr env, cl_index tentative_new_size)
{
  cl_index top = env->stack_top - env->stack;
  cl_object *new_stack, *old_stack;
  cl_index safety_area = ecl_option_values[ECL_OPT_LISP_STACK_SAFETY_AREA];
  cl_index new_size = tentative_new_size + 2*safety_area;

  /* Round to page size */
  new_size = ((new_size + LISP_PAGESIZE - 1) / LISP_PAGESIZE) * LISP_PAGESIZE;

  if (ecl_unlikely(top > new_size)) {
    FEerror("Internal error: cannot shrink stack below stack top.",0);
  }

  old_stack = env->stack;
  new_stack = (cl_object *)ecl_alloc_atomic(new_size * sizeof(cl_object));

  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  memcpy(new_stack, old_stack, env->stack_size * sizeof(cl_object));
  env->stack_size = new_size;
  env->stack_limit_size = new_size - 2*safety_area;
  env->stack = new_stack;
  env->stack_top = env->stack + top;
  env->stack_limit = env->stack + (new_size - 2*safety_area);

  /* A stack always has at least one element. This is assumed by cl__va_start
   * and friends, which take a sp=0 to have no arguments.
   */
  if (top == 0) {
    *(env->stack_top++) = ecl_make_fixnum(0);
  }
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);

  return env->stack_top;
}

void
FEstack_underflow(void)
{
  FEerror("Internal error: stack underflow.",0);
}

void
FEstack_advance(void)
{
  FEerror("Internal error: stack advance beyond current point.",0);
}

cl_object *
ecl_stack_grow(cl_env_ptr env)
{
  return ecl_stack_set_size(env, env->stack_size + env->stack_size / 2);
}

cl_index
ecl_stack_push_values(cl_env_ptr env) {
  cl_index i = env->nvalues;
  cl_object *b = env->stack_top;
  cl_object *p = b + i;
  if (p >= env->stack_limit) {
    b = ecl_stack_grow(env);
    p = b + i;
  }
  env->stack_top = p;
  memcpy(b, env->values, i * sizeof(cl_object));
  return i;
}

void
ecl_stack_pop_values(cl_env_ptr env, cl_index n) {
  cl_object *p = env->stack_top - n;
  if (ecl_unlikely(p < env->stack))
    FEstack_underflow();
  env->nvalues = n;
  env->stack_top = p;
  memcpy(env->values, p, n * sizeof(cl_object));
}

cl_object
ecl_stack_frame_open(cl_env_ptr env, cl_object f, cl_index size)
{
  cl_object *base = env->stack_top;
  if (size) {
    if ((env->stack_limit - base) < size) {
      base = ecl_stack_set_size(env, env->stack_size + size);
    }
  }
  f->frame.t = t_frame;
  f->frame.stack = env->stack;
  f->frame.base = base;
  f->frame.size = size;
  f->frame.env = env;
  env->stack_top = (base + size);
  return f;
}

void
ecl_stack_frame_push(cl_object f, cl_object o)
{
  cl_env_ptr env = f->frame.env;
  cl_object *top = env->stack_top;
  if (top >= env->stack_limit) {
    top = ecl_stack_grow(env);
  }
  env->stack_top = ++top;
  *(top-1) = o;
  f->frame.base = top - (++(f->frame.size));
  f->frame.stack = env->stack;
}

void
ecl_stack_frame_push_values(cl_object f)
{
  cl_env_ptr env = f->frame.env;
  ecl_stack_push_values(env);
  f->frame.base = env->stack_top - (f->frame.size += env->nvalues);
  f->frame.stack = env->stack;
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
    ECL_STACK_SET_INDEX(f->frame.env, f->frame.base - f->frame.stack);
  }
}

/* ------------------------- BINDING STACK ---------------------------- */

void
ecl_bds_unwind_n(cl_env_ptr env, int n)
{
  while (n--) ecl_bds_unwind1(env);
}

static void
ecl_bds_set_size(cl_env_ptr env, cl_index new_size)
{
  ecl_bds_ptr old_org = env->bds_stack.org;
  cl_index limit = env->bds_stack.top - old_org;
  if (new_size <= limit) {
    FEerror("Cannot shrink the binding stack below ~D.", 1,
            ecl_make_unsigned_integer(limit));
  } else {
    cl_index margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
    ecl_bds_ptr org;
    env->bds_stack.limit_size = new_size - 2*margin;
    org = ecl_alloc_atomic(new_size * sizeof(*org));

    ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
    memcpy(org, old_org, (limit + 1) * sizeof(*org));
    env->bds_stack.top = org + limit;
    env->bds_stack.org = org;
    env->bds_stack.limit = org + (new_size - 2*margin);
    env->bds_stack.size = new_size;
    ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);

    ecl_dealloc(old_org);
  }
}

ecl_bds_ptr 
ecl_bds_overflow(void)
{
  static const char *stack_overflow_msg =
    "\n;;;\n;;; Binding stack overflow.\n"
    ";;; Jumping to the outermost toplevel prompt\n"
    ";;;\n\n";
  cl_env_ptr env = ecl_process_env();
  cl_index margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  cl_index size = env->bds_stack.size;
  ecl_bds_ptr org = env->bds_stack.org;
  ecl_bds_ptr last = org + size;
  if (env->bds_stack.limit >= last) {
    ecl_unrecoverable_error(env, stack_overflow_msg);
  }
  env->bds_stack.limit += margin;
  si_serror(6, @"Extend stack size",
            @'ext::stack-overflow', @':size', ecl_make_fixnum(size),
            @':type', @'ext::binding-stack');
  ecl_bds_set_size(env, size + (size / 2));
  return env->bds_stack.top;
}

void
ecl_bds_unwind(cl_env_ptr env, cl_index new_bds_top_index)
{
  ecl_bds_ptr new_bds_top = new_bds_top_index + env->bds_stack.org;
  ecl_bds_ptr bds = env->bds_stack.top;
  for (;  bds > new_bds_top;  bds--)
#ifdef ECL_THREADS
    ecl_bds_unwind1(env);
#else
    bds->symbol->symbol.value = bds->value;
#endif
  env->bds_stack.top = new_bds_top;
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

#ifdef ecl_bds_bind
# undef ecl_bds_bind
# undef ecl_bds_push
# undef ecl_bds_unwind1
#endif
#ifdef ecl_bds_read
# undef ecl_bds_read
# undef ecl_bds_set
# undef ecl_bds_ref
#endif

#ifdef ECL_THREADS
static cl_index
ecl_new_binding_index(cl_env_ptr env, cl_object symbol)
{
  cl_object pool;
  cl_index new_index = symbol->symbol.binding;
  if (new_index == ECL_MISSING_SPECIAL_BINDING) {
    pool = ecl_atomic_pop(&ecl_core.reused_indices);
    if (!Null(pool)) {
      new_index = ecl_fixnum(ECL_CONS_CAR(pool));
    } else {
      new_index = ecl_atomic_index_incf(&ecl_core.last_var_index);
    }
    symbol->symbol.binding = new_index;
  }
  ecl_set_finalizer_unprotected(symbol, ECL_T);
  return new_index;
}

static cl_object
ecl_extend_bindings_array(cl_object vector)
{
  cl_index new_size = ecl_core.last_var_index * 1.25;
  cl_object new_vector = si_make_vector(ECL_T, ecl_make_fixnum(new_size), ECL_NIL,
                                        ECL_NIL, ECL_NIL, ECL_NIL);
  si_fill_array_with_elt(new_vector, ECL_NO_TL_BINDING, ecl_make_fixnum(0), ECL_NIL);
  ecl_copy_subarray(new_vector, 0, vector, 0, vector->vector.dim);
  return new_vector;
}

static cl_index
invalid_or_too_large_binding_index(cl_env_ptr env, cl_object s)
{
  cl_index index = s->symbol.binding;
  if (index == ECL_MISSING_SPECIAL_BINDING) {
    index = ecl_new_binding_index(env, s);
  }
  if (index >= env->bds_stack.thread_local_bindings_size) {
    cl_object vector = env->bds_stack.bindings_array;
    env->bds_stack.bindings_array = vector = ecl_extend_bindings_array(vector);
    env->bds_stack.thread_local_bindings_size = vector->vector.dim;
    env->bds_stack.thread_local_bindings = vector->vector.self.t;
  }
  return index;
}
#endif /* ECL_THREADS */

/*
 * The following routines must match the inline forms in stacks.h
 */
void
ecl_bds_bind(cl_env_ptr env, cl_object s, cl_object v)
{
#ifdef ECL_THREADS
  cl_object *location;
  ecl_bds_ptr slot;
  cl_index index = s->symbol.binding;
  if (index >= env->bds_stack.thread_local_bindings_size) {
    index = invalid_or_too_large_binding_index(env,s);
  }
  location = env->bds_stack.thread_local_bindings + index;
  slot = env->bds_stack.top+1;
  if (slot >= env->bds_stack.limit) slot = ecl_bds_overflow();
  slot->symbol = ECL_DUMMY_TAG;
  AO_nop_full();
  ++env->bds_stack.top;
  ecl_disable_interrupts_env(env);
  slot->symbol = s;
  slot->value = *location;
  *location = v;
  ecl_enable_interrupts_env(env);
#else
  ecl_bds_check(env);
  ecl_bds_ptr slot = ++(env->bds_stack.top);
  ecl_disable_interrupts_env(env);
  slot->symbol = s;
  slot->value = s->symbol.value;
  s->symbol.value = v;
  ecl_enable_interrupts_env(env);
#endif
}

void
ecl_bds_push(cl_env_ptr env, cl_object s)
{
#ifdef ECL_THREADS
  cl_object *location;
  ecl_bds_ptr slot;
  cl_index index = s->symbol.binding;
  if (index >= env->bds_stack.thread_local_bindings_size) {
    index = invalid_or_too_large_binding_index(env,s);
  }
  location = env->bds_stack.thread_local_bindings + index;
  slot = env->bds_stack.top+1;
  if (slot >= env->bds_stack.limit) slot = ecl_bds_overflow();
  slot->symbol = ECL_DUMMY_TAG;
  AO_nop_full();
  ++env->bds_stack.top;
  ecl_disable_interrupts_env(env);
  slot->symbol = s;
  slot->value = *location;
  if (*location == ECL_NO_TL_BINDING) *location = s->symbol.value;
  ecl_enable_interrupts_env(env);
#else
  ecl_bds_check(env);
  ecl_bds_ptr slot = ++(env->bds_stack.top);
  ecl_disable_interrupts_env(env);
  slot->symbol = s;
  slot->value = s->symbol.value;
  ecl_enable_interrupts_env(env);
#endif
}

void
ecl_bds_unwind1(cl_env_ptr env)
{
  cl_object s = env->bds_stack.top->symbol;
#ifdef ECL_THREADS
  cl_object *location = env->bds_stack.thread_local_bindings + s->symbol.binding;
  *location = env->bds_stack.top->value;
#else
  s->symbol.value = env->bds_stack.top->value;
#endif
  --env->bds_stack.top;
}

#ifdef ECL_THREADS
cl_object
ecl_bds_read(cl_env_ptr env, cl_object s)
{
  cl_index index = s->symbol.binding;
  if (index < env->bds_stack.thread_local_bindings_size) {
    cl_object x = env->bds_stack.thread_local_bindings[index];
    if (x != ECL_NO_TL_BINDING) return x;
  }
  return s->symbol.value;
}

cl_object *
ecl_bds_ref(cl_env_ptr env, cl_object s)
{
  cl_index index = s->symbol.binding;
  if (index < env->bds_stack.thread_local_bindings_size) {
    cl_object *location = env->bds_stack.thread_local_bindings + index;
    if (*location != ECL_NO_TL_BINDING)
      return location;
  }
  return &(s->symbol.value);
}

cl_object
ecl_bds_set(cl_env_ptr env, cl_object s, cl_object value)
{
  return *ecl_bds_ref(env, s) = value;
}
#endif /* ECL_THREADS */

/* ------------------------- INVOCATION STACK ------------------------- */

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
frs_set_size(cl_env_ptr env, cl_index new_size)
{
  ecl_frame_ptr old_org = env->frs_stack.org;
  cl_index limit = env->frs_stack.top - old_org;
  if (new_size <= limit) {
    FEerror("Cannot shrink frame stack below ~D.", 1,
            ecl_make_unsigned_integer(limit));
  } else {
    cl_index margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
    ecl_frame_ptr org;
    env->frs_stack.limit_size = new_size - 2*margin;
    org = ecl_alloc_atomic(new_size * sizeof(*org));

    ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
    memcpy(org, old_org, (limit + 1) * sizeof(*org));
    env->frs_stack.top = org + limit;
    env->frs_stack.org = org;
    env->frs_stack.limit = org + (new_size - 2*margin);
    env->frs_stack.size = new_size;
    ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);

    ecl_dealloc(old_org);
  }
}

static void
frs_overflow(void)              /* used as condition in list.d */
{
  static const char *stack_overflow_msg =
    "\n;;;\n;;; Frame stack overflow.\n"
    ";;; Jumping to the outermost toplevel prompt\n"
    ";;;\n\n";
  cl_env_ptr env = ecl_process_env();
  cl_index margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  cl_index size = env->frs_stack.size;
  ecl_frame_ptr org = env->frs_stack.org;
  ecl_frame_ptr last = org + size;
  if (env->frs_stack.limit >= last) {
    ecl_unrecoverable_error(env, stack_overflow_msg);
  }
  env->frs_stack.limit += margin;
  si_serror(6, @"Extend stack size",
            @'ext::stack-overflow', @':size', ecl_make_fixnum(size),
            @':type', @'ext::frame-stack');
  frs_set_size(env, size + size / 2);
}

ecl_frame_ptr
_ecl_frs_push(cl_env_ptr env)
{
  /* We store a dummy tag first, to make sure that it is safe to
   * interrupt this method with a call to ecl_unwind. Otherwise, a
   * stray ECL_PROTECT_TAG will lead to segfaults. AO_nop_full is
   * needed to ensure that the CPU doesn't reorder the memory
   * stores. */
  ecl_frame_ptr output = env->frs_stack.top+1;
  if (output >= env->frs_stack.limit) {
    frs_overflow();
    output = env->frs_stack.top+1;
  }
  output->frs_val = ECL_DUMMY_TAG;
  AO_nop_full();
  ++env->frs_stack.top;
  output->frs_bds_top_index = env->bds_stack.top - env->bds_stack.org;
  output->frs_ihs = env->ihs_stack.top;
  output->frs_sp = ECL_STACK_INDEX(env);
  return output;
}

void
ecl_unwind(cl_env_ptr env, ecl_frame_ptr fr)
{
  env->frs_stack.nlj_fr = fr;
  ecl_frame_ptr top = env->frs_stack.top;
  while (top != fr && top->frs_val != ECL_PROTECT_TAG){
    top->frs_val = ECL_DUMMY_TAG;
    --top;
  }
  env->ihs_stack.top = top->frs_ihs;
  ecl_bds_unwind(env, top->frs_bds_top_index);
  ECL_STACK_SET_INDEX(env, top->frs_sp);
  env->frs_stack.top = top;
  ecl_longjmp(env->frs_stack.top->frs_jmpbuf, 1);
  /* never reached */
}

ecl_frame_ptr
frs_sch (cl_object frame_id)
{
  cl_env_ptr env = ecl_process_env();
  ecl_frame_ptr top;
  for (top = env->frs_stack.top;  top >= env->frs_stack.org;  top--)
    if (top->frs_val == frame_id)
      return(top);
  return(NULL);
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
  ecl_return1(env, ecl_make_fixnum(get_frame_ptr(arg)->frs_bds_top_index));
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
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
    frs_set_size(env, the_size + 2*margin);
  } else if (type == @'ext::binding-stack') {
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
    ecl_bds_set_size(env, the_size + 2*margin);
  } else if (type == @'ext::c-stack') {
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
    cs_set_size(env, the_size + 2*margin);
  } else if (type == @'ext::lisp-stack') {
    cl_index the_size = ecl_to_size(limit);
    ecl_stack_set_size(env, the_size);
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
  else if (type == @'ext::c-stack')
    output = env->cs_limit_size;
  else if (type == @'ext::lisp-stack')
    output = env->stack_limit_size;
  else if (type == @'ext::heap-size') {
    /* size_t can be larger than cl_index */
    ecl_return1(env, ecl_make_unsigned_integer(ecl_core.max_heap_size));
  }

  ecl_return1(env, ecl_make_unsigned_integer(output));
}

cl_object
si_reset_margin(cl_object type)
{
  cl_env_ptr env = ecl_process_env();
  if (type == @'ext::frame-stack')
    frs_set_size(env, env->frs_stack.size);
  else if (type == @'ext::binding-stack')
    ecl_bds_set_size(env, env->bds_stack.size);
  else if (type == @'ext::c-stack')
    cs_set_size(env, env->cs_size);
  else
    ecl_return1(env, ECL_NIL);

  ecl_return1(env, ECL_T);
}

void
init_stacks(cl_env_ptr env)
{
  static struct ecl_ihs_frame ihs_org = { NULL, NULL, NULL, 0};
  cl_index size, margin;
  /* frame stack */
  margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  size = ecl_option_values[ECL_OPT_FRAME_STACK_SIZE] + 2 * margin;
  env->frs_stack.size = size;
  env->frs_stack.org = (ecl_frame_ptr)ecl_alloc_atomic(size * sizeof(*env->frs_stack.org));
  env->frs_stack.top = env->frs_stack.org-1;
  env->frs_stack.limit = &env->frs_stack.org[size - 2*margin];
  /* bind stack */
  margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  size = ecl_option_values[ECL_OPT_BIND_STACK_SIZE] + 2 * margin;
  env->bds_stack.size = size;
  env->bds_stack.org = (ecl_bds_ptr)ecl_alloc_atomic(size * sizeof(*env->bds_stack.org));
  env->bds_stack.top = env->bds_stack.org-1;
  env->bds_stack.limit = &env->bds_stack.org[size - 2*margin];
  /* ihs stack */
  env->ihs_stack.top = &ihs_org;
  ihs_org.function = ECL_NIL;
  ihs_org.lex_env = ECL_NIL;
  ihs_org.index = 0;
  /* lisp stack */
  env->stack = NULL;
  env->stack_top = NULL;
  env->stack_limit = NULL;
  env->stack_size = 0;
  ecl_stack_set_size(env, ecl_option_values[ECL_OPT_LISP_STACK_SIZE]);
}
