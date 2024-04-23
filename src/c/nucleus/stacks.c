/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * stacks.c - lisp stacks
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2024 Daniel Kochmański
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

/* -- General purpose LISP stack -------------------------------------------- */

cl_object *
ecl_stack_init(cl_object self, cl_index size, cl_index margin)
{
  cl_index limit_size = size-2*margin;
  self->stack.t = t_stack;
  self->stack.size = size;
  self->stack.limit_size = limit_size;
  self->stack.org = (cl_object *)ecl_malloc(size * sizeof(cl_object));
  self->stack.top = self->stack.org;
  self->stack.limit = &self->stack.org[limit_size];
  return self->stack.top;
}

cl_object *
ecl_stack_resize(cl_object self, cl_index size, cl_index margin)
{
  cl_index top = self->stack.top - self->stack.org;
  cl_index limit_size = size-2*margin;
  cl_object *new_stack;
  new_stack = (cl_object *)ecl_realloc(self->stack.org, size * sizeof(cl_object));
  self->stack.size = size;
  self->stack.limit_size = limit_size;
  self->stack.org = new_stack;
  self->stack.top = self->stack.org + top;
  self->stack.limit = &self->stack.org[limit_size];
  return self->stack.top;
}

cl_index
ecl_stack_index(cl_object self) {
  return self->stack.top - self->stack.org;
}

cl_object *
ecl_stack_unwind(cl_object self, cl_index ndx)
{
  self->stack.top = self->stack.org + ndx;
  return self->stack.top;
}

cl_object *
ecl_stack_tangle(cl_object self, cl_object them)
{
  cl_index ndx = ecl_stack_index(them);
  ecl_copy(self->stack.org, them->stack.org, ndx);
  self->stack.top = self->stack.org + ndx;
  return self->stack.top;
}

cl_object
ecl_stack_push(cl_object self, cl_object elt)
{
  cl_object *new_top = self->stack.top;
  if (ecl_unlikely(new_top >= self->stack.limit)) {
    cl_index old_size = self->stack.size;
    cl_index margin = old_size - self->stack.limit_size;
    cl_index new_size = old_size + old_size/2;
    new_top = ecl_stack_resize(self, new_size, margin);
  }
  self->stack.top = new_top+1;
  *new_top = (elt);
  return self;
}

cl_object
ecl_stack_pop(cl_object self)
{
  if (ecl_unlikely(self->stack.top <= self->stack.org))
    ecl_internal_error("ecl_pop: stack underflow");
  return *(--(self->stack.top));
}

cl_object
ecl_stack_dup(cl_object self)
{
  cl_object elt;
  if (ecl_unlikely(self->stack.top <= self->stack.org))
    ecl_internal_error("ecl_dup: empty stack");
  elt = ECL_STACK_TOP(self);
  ecl_stack_push(self, elt);
  return elt;
}

cl_object
ecl_stack_del(cl_object self, cl_object elt)
{
  cl_index idx;
  cl_index ndx = ECL_STACK_NDX(self);
  cl_object *v = self->stack.org;
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
  return *(--(self->stack.top));
}

void
ecl_stack_grow(cl_object self, cl_index n)
{
  self->stack.top += n;
}

void
ecl_stack_drop(cl_object self, cl_index n)
{
  self->stack.top -= n;
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
  ecl_cerror(ECL_EX_FRS_OVR, ecl_make_fixnum(limit_size), ECL_YES);
}

void
ecl_unwind(cl_env_ptr env, ecl_frame_ptr fr)
{
  env->frs_stack.nlj_fr = fr;
  ecl_frame_ptr top = env->frs_stack.top;
  cl_object vms = ecl_cast_ptr(cl_object,&env->vms_stack);
  while (top != fr && top->frs_val != ECL_PROTECT_TAG){
    top->frs_val = ECL_DUMMY_TAG;
    --top;
  }
  env->ihs_stack.top = top->frs_ihs;
  ecl_bds_unwind(env, top->frs_bds_ndx);
  ecl_stack_unwind(vms, top->frs_vms_ndx);
  env->frs_stack.top = top;
  ecl_longjmp(env->frs_stack.top->frs_jmpbuf, 1);
  /* never reached */
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
  output->frs_vms_ndx = ECL_VMS_NDX(env);
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
  cl_index limit_size = env->bds_stack.limit_size;
  ecl_bds_ptr org = env->bds_stack.org;
  ecl_bds_ptr last = org + size;
  if (env->bds_stack.limit >= last) {
    ecl_internal_error(stack_overflow_msg);
  }
  env->bds_stack.limit += margin;
  ecl_cerror(ECL_EX_BDS_OVR, ecl_make_fixnum(limit_size), ECL_YES);
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
    cl_index old_size = env->bds_stack.tl_bindings_size;
    cl_index new_size = ecl_core.last_var_index * 1.25;
    cl_object *old_vector = env->bds_stack.tl_bindings;
    cl_object *new_vector = ecl_realloc(old_vector, new_size*sizeof(cl_object*));
    while(old_size < new_size) {
      new_vector[old_size++] = ECL_NO_TL_BINDING;
    }
    env->bds_stack.tl_bindings = new_vector;
    env->bds_stack.tl_bindings_size = new_size;
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

/* --------------------------------------------------------------------------- */

void
init_early_stacks(cl_env_ptr env)
{
  frs_init(env);
  bds_init(env);
  /* ihs_init(env); */
  /* vms_init(env); */
}
