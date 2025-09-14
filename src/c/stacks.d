/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.d - runtime, binding, history and frame stacks
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
#include <signal.h>
#include <string.h>
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/time.h>
# include <sys/resource.h>
#endif
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/stack-resize.h>

/* -- C Stack ---------------------------------------------------------------- */
void
ecl_cs_init(cl_env_ptr env)
{
  volatile char foo = 0;
  cl_index margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
  cl_index new_size = ecl_option_values[ECL_OPT_C_STACK_SIZE];
  cl_index max_size = new_size;
#ifdef GBC_BOEHM
  struct GC_stack_base base;
  if (GC_get_stack_base(&base) == GC_SUCCESS)
    env->c_stack.org = (char*)base.mem_base;
  else
    env->c_stack.org = (char*)(&env);
#else
  /* Rough estimate. Not very safe. We assume that cl_boot() is invoked from the
   * main() routine of the program. */
  env->c_stack.org = (char*)(&env);
#endif
#ifdef ECL_CAN_SET_STACK_SIZE
  {
    struct rlimit rl;
    if (!getrlimit(RLIMIT_STACK, &rl)) {
      if (new_size > rl.rlim_cur) {
        rl.rlim_cur = (new_size > rl.rlim_max) ? rl.rlim_max : new_size;
        if (setrlimit(RLIMIT_STACK, &rl))
          ecl_internal_error("Can't set the size of the C stack");
      }
    } else {
      rl.rlim_cur = new_size;
      rl.rlim_max = max_size;
    }
    if (rl.rlim_cur == 0 || rl.rlim_cur == RLIM_INFINITY || rl.rlim_cur > (cl_index)(-1)) {
      /* Either getrlimit failed or returned nonsense, either way we don't
       * know the stack size. Use a default of 1 MB and hope for the best. */
      new_size = 1048576;
      max_size = 1048576;
    } else {
      new_size = rl.rlim_cur;
      max_size = rl.rlim_max;
    }
  }
#endif
  env->c_stack.limit_size = new_size - 2*margin;
  env->c_stack.size = new_size;
  env->c_stack.max_size = max_size;
#ifdef ECL_DOWN_STACK
  env->c_stack.max = env->c_stack.org - new_size;
  if (&foo > (env->c_stack.org - new_size) + 16) {
    env->c_stack.limit = (env->c_stack.org - new_size) + (2*margin);
    if (env->c_stack.limit < env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  } else {
    ecl_internal_error("Can't set the size of the C stack: sanity check failed.");
  }
#else
  env->c_stack.max = env->c_stack.org + new_size;
  if (&foo < (env->c_stack.org + new_size) - 16) {
    env->c_stack.limit = (env->c_stack.org + new_size) - (2*margin);
    if (env->c_stack.limit > env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  } else {
    ecl_internal_error("Can't set the size of the C stack: sanity check failed.");
  }
#endif
}

void
ecl_cs_set_size(cl_env_ptr env, cl_index new_size)
{
  volatile char foo = 0;
  cl_index margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
  if (new_size > env->c_stack.max_size)
    new_size = env->c_stack.max_size;
#ifdef ECL_CAN_SET_STACK_SIZE
  {
    struct rlimit rl;
    if (!getrlimit(RLIMIT_STACK, &rl)) {
      if (new_size > rl.rlim_cur) {
        rl.rlim_cur = (new_size > rl.rlim_max) ? rl.rlim_max : new_size;
        if (setrlimit(RLIMIT_STACK, &rl))
          ecl_internal_error("Can't set the size of the C stack");
      }
    } else {
      rl.rlim_cur = new_size;
    }
    if (rl.rlim_cur == 0 || rl.rlim_cur == RLIM_INFINITY || rl.rlim_cur > (cl_index)(-1)) {
      /* Either getrlimit failed or returned nonsense, either way we don't know
       * the stack size. Use a default of 1 MB and hope for the best. */
      new_size = 1048576;
    } else {
      new_size = rl.rlim_cur;
    }
  }
#endif
  env->c_stack.limit_size = new_size - 2*margin;
  env->c_stack.size = new_size;
#ifdef ECL_DOWN_STACK
  env->c_stack.max = env->c_stack.org - new_size;
  if (&foo > (env->c_stack.org - new_size) + 16) {
    env->c_stack.limit = (env->c_stack.org - new_size) + (2*margin);
    if (env->c_stack.limit < env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  } else {
    ecl_internal_error("Can't set the size of the C stack: sanity check failed.");
  }
#else
  env->c_stack.max = env->c_stack.org + new_size;
  if (&foo < (env->c_stack.org + new_size) - 16) {
    env->c_stack.limit = (env->c_stack.org + new_size) - (2*margin);
    if (env->c_stack.limit > env->c_stack.max)
      env->c_stack.max = env->c_stack.limit;
  } else {
    ecl_internal_error("Can't set the size of the C stack: sanity check failed.");
  }
#endif
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
    CEstack_overflow(@'ext::c-stack', ecl_make_fixnum(size), ECL_T);
  else
    CEstack_overflow(@'ext::c-stack', ecl_make_fixnum(size), ECL_NIL);
}

/* -- Data stack ------------------------------------------------------------ */

static void
run_init(cl_env_ptr env)
{
  cl_index size, limit_size, margin;
  margin = ecl_option_values[ECL_OPT_LISP_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_LISP_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->run_stack.org = (cl_object *)ecl_malloc(size * sizeof(cl_object));
  env->run_stack.top = env->run_stack.org;
  env->run_stack.limit = &env->run_stack.org[limit_size];
  env->run_stack.size = size;
  env->run_stack.limit_size = limit_size;
  /* A stack always has at least one element. This is assumed by cl__va_start
     and friends, which take a sp=0 to have no arguments. */
  *(env->run_stack.top++) = ecl_make_fixnum(0);
}

void
ecl_data_stack_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_LISP_STACK_SAFETY_AREA];
  cl_object *old_org = env->run_stack.org;
  cl_object *new_org = NULL;
  cl_index osize = env->run_stack.size;
  cl_index nsize = new_lim_size + 2*margin;
  cl_index current_size = env->run_stack.top - old_org;
  if (current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  new_org = ecl_realloc(old_org,
                        osize * sizeof(*old_org),
                        nsize * sizeof(*old_org));
  env->run_stack.org = new_org;
  env->run_stack.top = new_org + current_size;
  env->run_stack.limit = new_org + new_lim_size;
  /* Update indexes */
  env->run_stack.size = nsize;
  env->run_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

cl_object *
ecl_data_stack_grow(cl_env_ptr env)
{
  ecl_data_stack_set_limit(env, env->run_stack.limit_size + env->run_stack.limit_size / 2);
  return env->run_stack.top;
}

cl_index
ecl_data_stack_push_values(cl_env_ptr env) {
  cl_index i = env->nvalues;
  cl_object *b = env->run_stack.top;
  cl_object *p = b + i;
  while (p >= env->run_stack.limit) {
    b = ecl_data_stack_grow(env);
    p = b + i;
  }
  env->run_stack.top = p;
  ecl_copy(b, env->values, i * sizeof(cl_object));
  return i;
}

void
ecl_data_stack_pop_values(cl_env_ptr env, cl_index n) {
  cl_object *p = env->run_stack.top - n;
  if (ecl_unlikely(p < env->run_stack.org))
    ecl_internal_error("data stack: stack underflow.");
  env->nvalues = n;
  env->run_stack.top = p;
  ecl_copy(env->values, p, n * sizeof(cl_object));
}

/* A stack frame denotes a slice of the lisp stack [BASE,BASE+SIZE]. Between
   these two values we maintain a stack pointer SP that shows where we push and
   pop values when we use the frame. There are two nuances to keep in mind:

   1. When we try to push-extend to the frame, it is possible only if the stack
   top is aligned with the stack frame end: TOP_INDEX = SP = BASE+SIZE. This is
   to avoid a situation where we override a newer frame.

   2. When the stack top is aligned with the stack frame end, then push and pop
   modifies the lisp stack TOP and the frame's SP and SIZE. This ensures that we
   can use the topmost stack frame as if it were the stack, but also that we can
   use some inner frames without corrupting it.

   Note that direct stack operations do not update existing frames, so it is
   still possible to corrupt a stack frame if not carful. -- jd 2025-05-29 */

cl_object
ecl_stack_frame_open(cl_env_ptr env, cl_object f, cl_index size)
{
  cl_object *base = env->run_stack.top;
  cl_index bindex;
  if (size) {
    if ((env->run_stack.limit - base) < size) {
      ecl_data_stack_set_limit(env, env->run_stack.limit_size + size);
      base = env->run_stack.top;
    }
  }
  bindex = ECL_STACK_INDEX(env);
  f->frame.t = t_frame;
  f->frame.opened = 1;
  f->frame.base = bindex;
  f->frame.size = size;
  f->frame.sp = bindex;
  f->frame.env = env;
  env->run_stack.top = (base + size);
  return f;
}

void
ecl_stack_frame_push(cl_object f, cl_object o)
{
  cl_env_ptr the_env = f->frame.env;
  cl_object *frame_top = ECL_STACK_FRAME_TOP(f);
  cl_index limit_index = f->frame.base + f->frame.size;
  if (f->frame.sp < limit_index) {
    *frame_top = o;
    f->frame.sp++;
  } else if (frame_top == the_env->run_stack.top) {
    f->frame.sp++;
    f->frame.size++;
    ECL_STACK_PUSH(the_env, o);
  } else {
    ecl_internal_error("ecl_stack_frame_pop: frame overflow.");
  }
}

cl_object
ecl_stack_frame_pop(cl_object f)
{
  cl_env_ptr the_env = f->frame.env;
  cl_object *frame_top = ECL_STACK_FRAME_TOP(f);
  if (f->frame.sp <= f->frame.base) {
    ecl_internal_error("ecl_stack_frame_pop: frame underflow.");
  } else if (frame_top == the_env->run_stack.top) {
    f->frame.sp--;
    f->frame.size--;
    return ECL_STACK_POP_UNSAFE(the_env);
  } else {
    f->frame.sp--;
    return *ECL_STACK_FRAME_TOP(f);
  }
}

void
ecl_stack_frame_push_values(cl_object f)
{
  cl_env_ptr the_env = f->frame.env;
  cl_index limit_index = f->frame.base + f->frame.size;
  cl_index vals_length = the_env->nvalues;
  cl_index value_index = f->frame.sp + vals_length;
  cl_object *frame_top = ECL_STACK_FRAME_TOP(f);
  if (value_index <= limit_index) {
    ecl_copy(frame_top, the_env->values, vals_length * sizeof(cl_object));
    f->frame.sp = value_index;
  } else if (frame_top == the_env->run_stack.top) {
    f->frame.sp = value_index;
    f->frame.size = value_index - f->frame.base;
    ecl_data_stack_push_values(the_env);
  } else {
    ecl_internal_error("ecl_stack_frame_push: frame overflow.");
  }
}

cl_object
ecl_stack_frame_pop_values(cl_object f)
{
  cl_env_ptr the_env = f->frame.env;
  cl_index top_size = f->frame.sp - f->frame.base;
  cl_index n = top_size % ECL_MULTIPLE_VALUES_LIMIT;
  cl_object *frame_top = ECL_STACK_FRAME_TOP(f), result;
  if (frame_top == the_env->run_stack.top) {
    ecl_data_stack_pop_values(the_env, n);
    f->frame.sp -= n;
    f->frame.size -= n;
    return the_env->values[0];
  } else {
    the_env->nvalues = n;
    the_env->values[0] = result = ECL_NIL;
    while (n--) {
      the_env->values[n] = result = ECL_STACK_FRAME_REF(f, n);
    }
    f->frame.sp -= n;
    return result;
  }
}

void
ecl_stack_frame_close(cl_object f)
{
  if (f->frame.opened) {
    f->frame.opened = 0;
    ECL_STACK_UNWIND(f->frame.env, f->frame.base);
  }
}

/* -- Binding stack ---------------------------------------------------------- */

static void
bds_init(cl_env_ptr env)
{
  cl_index size, margin, limit_size;
  margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_BIND_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->bds_stack.org = (ecl_bds_ptr)ecl_malloc(size * sizeof(*env->bds_stack.org));
  env->bds_stack.top = env->bds_stack.org-1;
  env->bds_stack.limit = &env->bds_stack.org[limit_size];
  env->bds_stack.size = size;
  env->bds_stack.limit_size = limit_size;
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
    ecl_internal_error(stack_overflow_msg);
  }
  env->bds_stack.limit += margin;
  CEstack_overflow(@'ext::binding-stack', ecl_make_fixnum(size), ECL_T);
  return env->bds_stack.top;
}

void
ecl_bds_unwind(cl_env_ptr env, cl_index new_bds_ndx)
{
  ecl_bds_ptr new_bds_top = env->bds_stack.org + new_bds_ndx;
  ecl_bds_ptr bds = env->bds_stack.top;
  for (;  bds > new_bds_top;  bds--)
#ifdef ECL_THREADS
    ecl_bds_unwind1(env);
#else
    bds->symbol->symbol.value = bds->value;
#endif
  env->bds_stack.top = new_bds_top;
}

void
ecl_bds_unwind_n(cl_env_ptr env, int n)
{
  while (n--) ecl_bds_unwind1(env);
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
    pool = ecl_atomic_pop(&cl_core.reused_indices);
    if (!Null(pool)) {
      new_index = ecl_fixnum(ECL_CONS_CAR(pool));
    } else {
      new_index = ecl_atomic_index_incf(&cl_core.last_var_index);
    }
    symbol->symbol.binding = new_index;
  }
  return new_index;
}

static cl_index
invalid_or_too_large_binding_index(cl_env_ptr env, cl_object s)
{
  cl_index index = s->symbol.binding;
  if (index == ECL_MISSING_SPECIAL_BINDING) {
    index = ecl_new_binding_index(env, s);
  }
  if (index >= env->bds_stack.tl_bindings_size) {
    cl_index osize = env->bds_stack.tl_bindings_size;
    cl_index nsize = cl_core.last_var_index * 1.25;
    cl_object *old_vector = env->bds_stack.tl_bindings;
    cl_object *new_vector = ecl_realloc(old_vector,
                                        osize*sizeof(cl_object*),
                                        nsize*sizeof(cl_object*));
    while(osize < nsize) {
      new_vector[osize++] = ECL_NO_TL_BINDING;
    }
    env->bds_stack.tl_bindings = new_vector;
    env->bds_stack.tl_bindings_size = nsize;
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
  if (index >= env->bds_stack.tl_bindings_size) {
    index = invalid_or_too_large_binding_index(env,s);
  }
  location = env->bds_stack.tl_bindings + index;
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
  if (index >= env->bds_stack.tl_bindings_size) {
    index = invalid_or_too_large_binding_index(env,s);
  }
  location = env->bds_stack.tl_bindings + index;
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
  cl_object *location = env->bds_stack.tl_bindings + s->symbol.binding;
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
  if (index < env->bds_stack.tl_bindings_size) {
    cl_object x = env->bds_stack.tl_bindings[index];
    if (x != ECL_NO_TL_BINDING) return x;
  }
  return s->symbol.value;
}

cl_object *
ecl_bds_ref(cl_env_ptr env, cl_object s)
{
  cl_index index = s->symbol.binding;
  if (index < env->bds_stack.tl_bindings_size) {
    cl_object *location = env->bds_stack.tl_bindings + index;
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

void
ecl_bds_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_BIND_STACK_SAFETY_AREA];
  ecl_bds_ptr old_org = env->bds_stack.org;
  ecl_bds_ptr new_org = NULL;
  cl_index osize = env->bds_stack.size;
  cl_index nsize = new_lim_size + 2*margin;
  cl_index current_size = env->bds_stack.top - old_org;
  if (current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  new_org = ecl_realloc(old_org,
                        osize * sizeof(*old_org),
                        nsize * sizeof(*old_org));
  env->bds_stack.org = new_org;
  env->bds_stack.top = new_org + current_size;
  env->bds_stack.limit = new_org + new_lim_size;
  /* Update indexes */
  env->bds_stack.size = nsize;
  env->bds_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

/* -- Invocation stack ------------------------------------------------------- */
static void
ihs_init(cl_env_ptr env)
{
  static struct ecl_ihs_frame ihs_org = { NULL, NULL, NULL, 0};
  env->ihs_stack.top = &ihs_org;
  ihs_org.function = ECL_NIL;
  ihs_org.lex_env = ECL_NIL;
  ihs_org.index = 0;
}

/* -- Frame stack ------------------------------------------------------------ */

static void
frs_init(cl_env_ptr env)
{
  cl_index size, margin, limit_size;
  margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  limit_size = ecl_option_values[ECL_OPT_FRAME_STACK_SIZE];
  size = limit_size + 2 * margin;
  env->frs_stack.org = (ecl_frame_ptr)ecl_malloc(size * sizeof(*env->frs_stack.org));
  env->frs_stack.top = env->frs_stack.org-1;
  env->frs_stack.limit = &env->frs_stack.org[limit_size];
  env->frs_stack.size = size;
  env->frs_stack.limit_size = limit_size;
}

void
ecl_frs_set_limit(cl_env_ptr env, cl_index new_lim_size)
{
  cl_index margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  ecl_frame_ptr old_org = env->frs_stack.org;
  ecl_frame_ptr new_org = NULL;
  cl_index osize = env->frs_stack.size;
  cl_index nsize = new_lim_size + 2*margin;
  cl_index current_size = env->frs_stack.top - old_org;
  if (current_size > new_lim_size)
    ecl_internal_error("Cannot shrink frame stack below its minimal element");
  ECL_STACK_RESIZE_DISABLE_INTERRUPTS(env);
  new_org = ecl_realloc(old_org,
                        osize * sizeof(*old_org),
                        nsize * sizeof(*old_org));
  env->frs_stack.org = new_org;
  env->frs_stack.top = new_org + current_size;
  env->frs_stack.limit = new_org + new_lim_size;
  /* Update indexes. */
  env->frs_stack.size = nsize;
  env->frs_stack.limit_size = new_lim_size;
  ECL_STACK_RESIZE_ENABLE_INTERRUPTS(env);
}

static void
frs_overflow(void)
{
  static const char *stack_overflow_msg =
    "\n;;;\n;;; Frame stack overflow.\n"
    ";;; Jumping to the outermost toplevel prompt\n"
    ";;;\n\n";
  cl_env_ptr env = ecl_process_env();
  cl_index margin = ecl_option_values[ECL_OPT_FRAME_STACK_SAFETY_AREA];
  cl_index size = env->frs_stack.size;
  cl_index limit_size = env->frs_stack.limit_size;
  ecl_frame_ptr org = env->frs_stack.org;
  ecl_frame_ptr last = org + size;
  if (env->frs_stack.limit >= last) {
    ecl_internal_error(stack_overflow_msg);
  }
  env->frs_stack.limit += margin;
  CEstack_overflow(@'ext::frame-stack', ecl_make_fixnum(limit_size), ECL_T);
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
  output->frs_bds_ndx = env->bds_stack.top - env->bds_stack.org;
  output->frs_run_ndx = ECL_STACK_INDEX(env);
  output->frs_ihs = env->ihs_stack.top;
  return output;
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

/* -- Initialization -------------------------------------------------------- */
cl_object
init_stacks(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  if (the_env == cl_core.first_env) {
    cl_index idx;
    cl_object *vector = (cl_object *)ecl_malloc(1024*sizeof(cl_object*));
    for(idx=0; idx<1024; idx++) {
      vector[idx] = ECL_NO_TL_BINDING;
    }
    the_env->bds_stack.tl_bindings_size = 1024;
    the_env->bds_stack.tl_bindings = vector;
  }
#endif
  frs_init(the_env);
  bds_init(the_env);
  run_init(the_env);
  ihs_init(the_env);
  /* FIXME ecl_cs_init must be called from the thread entry point at the
     beginning to correctly determine the stack base. */
#if 0
  cs_init(the_env);
#endif
  return ECL_NIL;
}

cl_object
free_stacks(cl_env_ptr the_env)
{
#ifdef ECL_THREADS
  ecl_free(the_env->bds_stack.tl_bindings);
  the_env->bds_stack.tl_bindings_size = 0;
#endif
  ecl_free(the_env->run_stack.org);
  ecl_free(the_env->bds_stack.org);
  ecl_free(the_env->frs_stack.org);
  return ECL_NIL;
}

/* -- High level interface -------------------------------------------------- */

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
  ecl_bds_unwind(env, top->frs_bds_ndx);
  ECL_STACK_UNWIND(env, top->frs_run_ndx);
  env->frs_stack.top = top;
  ecl_longjmp(env->frs_stack.top->frs_jmpbuf, 1);
  /* never reached */
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

/* -- Bindings stack -------------------------------------------------------- */

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

/* -- Frame stack ----------------------------------------------------------- */

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

/* -- Invocation stack ------------------------------------------------------ */

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

/* -- General purpose stack implementation ----------------------------------- */

/* Stacks are based on actually adjustable simple vectors. */
cl_object
ecl_make_stack(cl_index size)
{
  cl_object x = ecl_malloc(sizeof(struct ecl_vector));
  x->vector.t = t_vector;
  x->vector.elttype = ecl_aet_object;
  x->vector.self.t = NULL;
  x->vector.displaced = ECL_NIL;
  x->vector.dim = size;
  x->vector.fillp = 0;
  x->vector.flags = ECL_FLAG_ADJUSTABLE | ECL_FLAG_HAS_FILL_POINTER;
  x->vector.self.t = (cl_object *)ecl_malloc(size * sizeof(cl_object));
  return x;
}
void
ecl_free_stack(cl_object self)
{
  ecl_free(self->vector.self.t);
  ecl_free(self);
}

void
ecl_stack_resize(cl_object self, cl_index nsize)
{
  cl_index osize = self->vector.dim;
  self->vector.self.t = (cl_object *)ecl_realloc(self->vector.self.t,
                                                 osize * sizeof(cl_object),
                                                 nsize * sizeof(cl_object));
  self->vector.dim = nsize;
}

void
stack_ensure_size(cl_object self, cl_index nsize)
{
  if (nsize >= self->vector.dim) {
    ecl_stack_resize(self, nsize);
  }
}

cl_index
ecl_stack_index(cl_object self) {
  return self->vector.fillp;
}

cl_object
ecl_stack_push(cl_object self, cl_object elt)
{
  cl_index fillp = self->vector.fillp;
  cl_index dim = self->vector.dim;
  if (ecl_unlikely(fillp == dim)) {
    cl_index new_dim = dim+dim/2+1;
    ecl_stack_resize(self, new_dim);
  }
  self->vector.self.t[self->vector.fillp++] = elt;
  return self;
}

cl_object
ecl_stack_del(cl_object self, cl_object elt)
{
  cl_index idx;
  cl_index ndx = self->vector.fillp;
  cl_object *v = self->vector.self.t;
  for(idx = 0; idx < ndx; idx++) {
    if (v[idx] == elt) {
      do { v[idx] = v[idx+1]; } while (++idx <= ndx);
      ecl_stack_popu(self);
      break;
    }
  }
  return self;
}

/* Unsafe operations */

cl_object
ecl_stack_popu(cl_object self)
{
  cl_object result = self->vector.self.t[--self->vector.fillp];
  self->vector.self.t[self->vector.fillp] = ECL_NIL;
  return result;
}

/* -- Lisp ops on stacks ---------------------------------------------------- */

cl_object
si_set_limit(cl_object type, cl_object limit)
{
  cl_env_ptr env = ecl_process_env();
  cl_index margin;
  if (type == @'ext::frame-stack') {
    cl_index current_size = env->frs_stack.top - env->frs_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink frame stack below ~D.", 1, limit);
    ecl_frs_set_limit(env, request_size);
  } else if (type == @'ext::binding-stack') {
    cl_index current_size = env->bds_stack.top - env->bds_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink binding stack below ~D.", 1, limit);
    ecl_bds_set_limit(env, request_size);
  } else if (type == @'ext::lisp-stack') {
    cl_index current_size = env->run_stack.top - env->run_stack.org;
    cl_index request_size = ecl_to_size(limit);
    if(current_size > request_size)
      FEerror("Cannot shrink lisp stack below ~D.", 1, limit);
    ecl_data_stack_set_limit(env, request_size);
  } else if (type == @'ext::c-stack') {
    cl_index the_size = ecl_to_size(limit);
    margin = ecl_option_values[ECL_OPT_C_STACK_SAFETY_AREA];
    ecl_cs_set_size(env, the_size + 2*margin);
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
    output = env->run_stack.limit_size;
  else if (type == @'ext::c-stack')
    output = env->c_stack.limit_size;
  else if (type == @'ext::heap-size') {
    /* size_t can be larger than cl_index */
    ecl_return1(env, ecl_make_unsigned_integer(cl_core.max_heap_size));
  }

  ecl_return1(env, ecl_make_unsigned_integer(output));
}
