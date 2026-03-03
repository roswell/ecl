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
#include <ecl/nucleus.h>
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
  if (env->c_stack.org == NULL) {
    /* Rough estimate. Not very safe. We assume that cl_boot() is invoked from
     * the main() routine of the program. */
    env->c_stack.org = (char*)(&env);
  }
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
    ecl_cerror(ECL_EX_CS_OVR, ecl_make_fixnum(size), ECL_T);
  else
    ecl_ferror(ECL_EX_CS_OVR, ecl_make_fixnum(size), ECL_NIL);
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
  new_org = ecl_resize(old_org,
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

/* Some clang versions miscompile the following function on x86_64.
 * Temporarily turn off optimizations here. */
#if defined(__clang__) && __clang_major__ >= 17 && __clang_major__ <= 19 && defined(__x86_64__)
[[clang::optnone]]
#endif
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

/* Some clang versions miscompile the following function on x86_64.
 * Temporarily turn off optimizations here. */
#if defined(__clang__) && __clang_major__ >= 17 && __clang_major__ <= 19 && defined(__x86_64__)
[[clang::optnone]]
#endif
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
  env->bds_stack.org = (ecl_bds_ptr)ecl_malloc(size * sizeof(cl_object*));
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
  cl_index limit_size = env->bds_stack.limit_size;
  ecl_bds_ptr org = env->bds_stack.org;
  ecl_bds_ptr last = org + size;
  if (env->bds_stack.limit >= last) {
    ecl_internal_error(stack_overflow_msg);
  }
  env->bds_stack.limit += margin;
  ecl_cerror(ECL_EX_BDS_OVR, ecl_make_fixnum(limit_size), ECL_T);
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
    pool = ecl_atomic_pop(&ecl_core.reused_indices);
    if (!Null(pool)) {
      new_index = ecl_fixnum(ECL_CONS_CAR(pool));
    } else {
      new_index = ecl_atomic_index_incf(&ecl_core.last_var_index);
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
    cl_index nsize = ecl_core.last_var_index * 1.25;
    cl_object *old_vector = env->bds_stack.tl_bindings;
    cl_object *new_vector = ecl_resize(old_vector,
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
  new_org = ecl_resize(old_org,
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
  new_org = ecl_resize(old_org,
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
  ecl_cerror(ECL_EX_FRS_OVR, ecl_make_fixnum(limit_size), ECL_T);
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

/* -- Module definition ------------------------------------------------------ */

static cl_object
create_stacks(void)
{
  cl_env_ptr the_env = ecl_core.first_env;
#ifdef ECL_THREADS
  cl_index idx;
  cl_object *vector = (cl_object *)ecl_malloc(1024*sizeof(cl_object*));
  for(idx=0; idx<1024; idx++) {
    vector[idx] = ECL_NO_TL_BINDING;
  }
  the_env->bds_stack.tl_bindings_size = 1024;
  the_env->bds_stack.tl_bindings = vector;
#endif
  the_env->c_stack.org = NULL;
  return ECL_NIL;
}

static cl_object
enable_stacks(void)
{
  return ECL_NIL;
}

static cl_object
init_env_stacks(cl_env_ptr the_env)
{
  frs_init(the_env);
  bds_init(the_env);
  run_init(the_env);
  ihs_init(the_env);
  the_env->c_stack.org = NULL;
  return ECL_NIL;
}

static cl_object
init_cpu_stacks(cl_env_ptr the_env)
{
  ecl_cs_init(the_env);
  return ECL_NIL;
}

static cl_object
free_cpu_stacks(cl_env_ptr the_env)
{
  return ECL_NIL;
}

static cl_object
free_env_stacks(cl_env_ptr the_env)
{
  ecl_free(the_env->run_stack.org);
  ecl_free(the_env->bds_stack.org);
  ecl_free(the_env->frs_stack.org);
  return ECL_NIL;
}

static cl_object
destroy_stacks(void)
{
  cl_env_ptr the_env = ecl_core.first_env;
#ifdef ECL_THREADS
  ecl_free(the_env->bds_stack.tl_bindings);
  the_env->bds_stack.tl_bindings_size = 0;
  the_env->bds_stack.tl_bindings = NULL;
#endif
  return ECL_NIL;
}

ecl_def_ct_base_string(str_stacks, "STACKS", 6, static, const);

static struct ecl_module module_stacks = {
  .t = t_module,
  .name = str_stacks,
  .create = create_stacks,
  .enable = enable_stacks,
  .init_env = init_env_stacks,
  .init_cpu = init_cpu_stacks,
  .free_cpu = free_cpu_stacks,
  .free_env = free_env_stacks,
  .disable = ecl_module_no_op,
  .destroy = destroy_stacks
};

cl_object ecl_module_stacks = (cl_object)&module_stacks;

/* -- General purpose stack implementation ----------------------------------- */

/* Stacks are based on actually adjustable simple vectors. */
cl_object
ecl_make_stack(cl_index size)
{
  /* XXX ecl_alloc flags=manual */
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
  self->vector.self.t = (cl_object *)ecl_resize(self->vector.self.t,
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

void
ecl_wipe_stack(cl_object self) {
  int i, fp=self->vector.fillp;
  self->vector.fillp = 0;
  for (i=0; i<fp; i++)
    self->vector.self.t[i] = ECL_NIL;
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
ecl_stack_pop(cl_object self)
{
  cl_index fillp = self->vector.fillp;
  cl_object elt = ECL_NIL;
  if (ecl_unlikely(fillp == 0)) {
    ecl_internal_error("ecl_stack_pop: stack underflow");
  }
  elt = self->vector.self.t[fillp-1];
  self->vector.self.t[fillp-1] = ECL_NIL;
  self->vector.fillp--;
  return elt;
}

cl_object
ecl_stack_psh(cl_object self, cl_object elt)
{
  cl_index fillp = self->vector.fillp;
  cl_index dim = self->vector.dim;
  if (ecl_unlikely(fillp == dim)) {
    ecl_internal_error("ecl_stack_psh: stack overflow");
  }
  self->vector.self.t[self->vector.fillp++] = elt;
  return self;
}

cl_object
ecl_stack_dup(cl_object self)
{
  cl_index fillp = self->vector.fillp;
  if (ecl_unlikely(fillp == 0)) {
    ecl_internal_error("ecl_stack_dup: empty stack");
  }
  ecl_stack_push(self, self->vector.self.t[fillp-1]);
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

cl_object
ecl_stack_pshu(cl_object self, cl_object elt)
{
  self->vector.self.t[self->vector.fillp++] = elt;
  return elt;
}

void
ecl_stack_grow(cl_object self, cl_index n)
{
  self->vector.fillp += n;
}

void
ecl_stack_drop(cl_object self, cl_index n)
{
  self->vector.fillp -= n;
}
