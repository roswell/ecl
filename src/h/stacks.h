/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
 * Copyright (c) 1990, Giuseppe Attardi.
 * Copyright (c) 2000, Juan Jose Garcia-Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* stacks.h -- Bind/Jump/Frame stacks. */

#ifndef ECL_STACKS_H
#define ECL_STACKS_H

#include <ecl/ecl_atomics.h>

#ifdef __cplusplus
extern "C" {
#endif

/***********
 * C STACK
 ***********/

#ifdef ECL_DOWN_STACK
#define ecl_cs_check(env,var) \
        if (ecl_unlikely((char*)(&var) <= (env)->c_stack.limit)) ecl_cs_overflow()
#else
#define ecl_cs_check(env,var) \
        if (ecl_unlikely((char*)(&var) >= (env)->c_stack.limit)) ecl_cs_overflow()
#endif

/*********************************************************
 * INTERRUPT SAFE STACK MANIPULATIONS
 *
 * The requirement for interruptible threads puts major
 * restrictions on the implementation of stack push/pop and unwind
 * routines. There are two principle requirements to be fulfilled:
 * The code which is executed during the interrupt must not
 * overwrite stack values (1) and it must be able to safely call stack
 * unwind functions (2).
 * The first requirement can be met in two distinct ways:
 * - Ordering the manipulations of the stack pointer and of the
 *   stack itself in the right manner. This means, when pushing in
 *   the stack first increase the stack pointer and then write the
 *   value and the other way round for popping from the stack. This
 *   method has the drawback that it requires insertions of
 *   memory barriers on modern processors, possibly impacting
 *   performance.
 * - Avoid overwriting stack values in the interrupt signal
 *   handler. This can be achieved by either increasing the stack
 *   pointer temporarily during the execution of the interrupt code
 *   or by saving/restoring the topmost stack value. However due to
 *   the second requirement, this simple method is sufficient only
 *   for the arguments stack.
 * The second requirement requires the stack to be in a consistent
 * state during the interrupt. The easiest solution would be to
 * disable interrupts during stack manipulations. Because of the
 * mprotect()-mechanism for fast interrupt dispatch, which does not
 * allow writes in the thread-local environment while interrupts
 * are disabled, this is unfortunately not possible. The solution
 * adopted for this case (bindings and frame stack) is to push a
 * dummy tag/symbol in the stack before any other manipulations are
 * done. This dummy tag/symbol will then be ignored while
 * unwinding.
 */

/**************
 * BIND STACK
 **************/

typedef struct ecl_bds_frame {
        cl_object symbol;       /*  symbol  */
        cl_object value;        /*  previous value of the symbol  */
} *ecl_bds_ptr;

#define ecl_bds_check(env) \
        (ecl_unlikely(env->bds_stack.top >= env->bds_stack.limit)? (ecl_bds_overflow(),1) : 0)

#define ECL_MISSING_SPECIAL_BINDING (~((cl_index)0))

extern ECL_API ecl_bds_ptr ecl_bds_overflow(void);
extern ECL_API void ecl_bds_bind(cl_env_ptr env, cl_object symbol, cl_object v);
extern ECL_API void ecl_bds_push(cl_env_ptr env, cl_object symbol);
extern ECL_API void ecl_bds_unwind1(cl_env_ptr env);
extern ECL_API void ecl_bds_unwind_n(cl_env_ptr env, int n);
#ifdef ECL_THREADS
extern ECL_API cl_object ecl_bds_read(cl_env_ptr env, cl_object s);
extern ECL_API cl_object *ecl_bds_ref(cl_env_ptr env, cl_object s);
extern ECL_API cl_object ecl_bds_set(cl_env_ptr env, cl_object s, cl_object v);
# define ECL_SYM_VAL(env,s) (ecl_bds_read(env,s))
# define ECL_SET(s,v) ((s)->symbol.value=(v))
# define ECL_SETQ(env,s,v) (*ecl_bds_ref(env,s)=(v))
#else
# define ECL_SYM_VAL(env,s) ((s)->symbol.value)
# define ECL_SET(s,v) ((s)->symbol.value=(v))
# define ECL_SETQ(env,s,v) ((s)->symbol.value=(v))
#endif

#ifdef ECL_THREADS
static inline void
ecl_bds_bind_inl(cl_env_ptr env, cl_object s, cl_object v)
{
        ecl_bds_ptr slot;
        cl_object *location;
        const cl_index index = s->symbol.binding;
        if (index >= env->bds_stack.tl_bindings_size) {
                ecl_bds_bind(env,s,v);
        } else {
                location = env->bds_stack.tl_bindings + index;
                slot = env->bds_stack.top+1;
                if (slot >= env->bds_stack.limit) slot = ecl_bds_overflow();
                /* First, we push a dummy symbol in the stack to
                 * prevent segfaults when we are interrupted with a
                 * call to ecl_bds_unwind. */
                slot->symbol = ECL_DUMMY_TAG;
                AO_nop_full();
                ++env->bds_stack.top;
                /* Then we disable interrupts to ensure that
                 * ecl_bds_unwind doesn't overwrite the symbol with
                 * some random value. */
                ecl_disable_interrupts_env(env);
                slot->symbol = s;
                slot->value = *location;
                *location = v;
                ecl_enable_interrupts_env(env);
        }
}

static inline void
ecl_bds_push_inl(cl_env_ptr env, cl_object s)
{
        ecl_bds_ptr slot;
        cl_object *location;
        const cl_index index = s->symbol.binding;
        if (index >= env->bds_stack.tl_bindings_size) {
                ecl_bds_push(env, s);
        } else {
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
        }
}

static inline void
ecl_bds_unwind1_inl(cl_env_ptr env)
{
        cl_object s = env->bds_stack.top->symbol;
        cl_object *location = env->bds_stack.tl_bindings + s->symbol.binding;
        *location = env->bds_stack.top->value;
        --env->bds_stack.top;
}

static inline cl_object
ecl_bds_read_inl(cl_env_ptr env, cl_object s)
{
        cl_index index = s->symbol.binding;
        if (index < env->bds_stack.tl_bindings_size) {
                cl_object x = env->bds_stack.tl_bindings[index];
                if (x != ECL_NO_TL_BINDING) return x;
        }
        return s->symbol.value;
}
static inline cl_object *
ecl_bds_ref_inl(cl_env_ptr env, cl_object s)
{
        cl_index index = s->symbol.binding;
        if (index < env->bds_stack.tl_bindings_size) {
                cl_object *location = env->bds_stack.tl_bindings + index;
                if (*location != ECL_NO_TL_BINDING) return location;
        }
        return &s->symbol.value;
}

# define ecl_bds_set(env,s,v) (*ecl_bds_ref_inl(env,s)=(v))
# define ecl_bds_read ecl_bds_read_inl

#else  /* ECL_THREADS */
static inline void
ecl_bds_bind_inl(cl_env_ptr env, cl_object s, cl_object v)
{
        ecl_bds_ptr slot;
        slot = ++env->bds_stack.top;
        if (slot >= env->bds_stack.limit) slot = ecl_bds_overflow();
        ecl_disable_interrupts_env(env);
        slot->symbol = s;
        slot->value = s->symbol.value;
        s->symbol.value = v;
        ecl_enable_interrupts_env(env);
}

static inline void
ecl_bds_push_inl(cl_env_ptr env, cl_object s)
{
        ecl_bds_ptr slot;
        slot = ++env->bds_stack.top;
        if (slot >= env->bds_stack.limit) slot = ecl_bds_overflow();
        ecl_disable_interrupts_env(env);
        slot->symbol = s;
        slot->value = s->symbol.value;
        ecl_enable_interrupts_env(env);
}

static inline void
ecl_bds_unwind1_inl(cl_env_ptr env)
{
        cl_object s = env->bds_stack.top->symbol;
        s->symbol.value = env->bds_stack.top->value;
        --env->bds_stack.top;
}
#endif  /* ECL_THREADS */

#define ecl_bds_bind ecl_bds_bind_inl
#define ecl_bds_push ecl_bds_push_inl
#define ecl_bds_unwind1 ecl_bds_unwind1_inl

/****************************
 * INVOCATION HISTORY STACK
 ****************************/

typedef struct ecl_ihs_frame {
        struct ecl_ihs_frame *next;
        cl_object function;
        cl_object lex_env;
        cl_index index;
        cl_index bds;
} *ecl_ihs_ptr;

#define ecl_ihs_push(env,rec,fun,lisp_env) do { \
        const cl_env_ptr __the_env = (env);     \
        ecl_ihs_ptr const r = (ecl_ihs_ptr const)(rec); \
        r->next=__the_env->ihs_stack.top;             \
        r->function=(fun);                            \
        r->lex_env=(lisp_env);                        \
        r->index=__the_env->ihs_stack.top->index+1;   \
        r->bds=__the_env->bds_stack.top - __the_env->bds_stack.org; \
        __the_env->ihs_stack.top = r;                               \
} while(0)

#define ecl_ihs_pop(env) do {                           \
        const cl_env_ptr __the_env = (env);             \
        ecl_ihs_ptr r = __the_env->ihs_stack.top;       \
        if (r) __the_env->ihs_stack.top = r->next;      \
} while(0)

/***************
 * FRAME STACK
 ***************/
/* Frames signal points in the code to which we can at any time jump.
 * Frames are established, for instance, by CATCH, BLOCK, TAGBODY,
 * LAMBDA, UNWIND-PROTECT, etc.
 *
 * Frames are established by ecl_frs_push(). For each call to ecl_frs_push()
 * there must be a corresponding ecl_frs_pop(). More precisely, since our
 * frame mechanism relies on the C stack and on the setjmp/longjmp
 * functions, any function that creates a frame must also destroy it
 * with ecl_frs_pop() before returning.
 *
 * Frames are identified by a value frs_val. This can be either a
 * unique identifier, created for each CATCH, BLOCK, etc, or a common
 * one ECL_PROTECT_TAG, used by UNWIND-PROTECT forms. The first type
 * of frames can be target of a search ecl_frs_sch() and thus one can jump
 * to them. The second type of frames are like barriers designed to
 * intercept the jumps to the outer frames and are called
 * automatically by the function unwind() whenever it jumps to a frame
 * which is beyond one of these barriers.
 */

typedef struct ecl_frame {
        jmp_buf         frs_jmpbuf;
        cl_object       frs_val;
        ecl_ihs_ptr     frs_ihs;
        cl_index        frs_bds_ndx;
        cl_index        frs_vms_ndx;
} *ecl_frame_ptr;

extern ECL_API ecl_frame_ptr _ecl_frs_push(cl_env_ptr);
#define ecl_frs_push(env,val) \
        ecl_frame_ptr __frame = _ecl_frs_push(env); \
        ecl_disable_interrupts_env(env); \
        __frame->frs_val = val; \
        int __ecl_frs_push_result = ecl_setjmp(__frame->frs_jmpbuf); \
        ecl_enable_interrupts_env(env)

#define ecl_frs_pop(env) ((env)->frs_stack.top--)
#define ecl_frs_pop_n(env,n) ((env)->frs_stack.top-=n)

/*******************
 * ARGUMENTS STACK
 *******************
 * Here we define how we handle the incoming arguments for a
 * function. Our calling conventions specify that at most
 * ECL_C_ARGUMENTS_LIMIT ar pushed onto the C stack. If the function
 * receives more than this number of arguments it will keep a copy of
 * _all_ those arguments _plus_ the remaining ones in the lisp
 * stack. The caller is responsible for storing and removing such
 * values.
 *
 * Given this structure, we need our own object for handling variable
 * argument list, ecl_va_list. This object joins the C data type for
 * handling vararg lists and a pointer to the lisp stack, in case the
 * arguments were passed there.
 *
 * Note that keeping a direct reference to the lisp stack effectively
 * locks it in memory, preventing the block from being garbage
 * collected if the stack grows -- at least until all references are
 * eliminated --. This is something we have to live with and which
 * is somehow unavoidable, given that function arguments have to be
 * stored somewhere.
 */

#define ecl_va_start(a,p,n,k) { \
        a[0].narg = (n)-(k); \
        va_start(a[0].args,p); \
        a[0].sp = ((n) <= ECL_C_ARGUMENTS_LIMIT)? 0 : _ecl_va_sp(k); }
#define ecl_va_arg(a) \
        (a[0].narg--,(a[0].sp? *(a[0].sp++) : va_arg(a[0].args,cl_object)))
#define ecl_va_copy(dest,orig) { \
        dest[0].narg = orig[0].narg; \
        dest[0].sp = orig[0].sp; \
        va_copy(dest[0].args,orig[0].args); \
}
#define ecl_va_end(a) \
        va_end(a[0].args)

/***********************
 * RETURN VALUES STACK
 ***********************/

#define ecl_nth_value(env,n) ((env)->values[n])
#define ecl_nvalues(env) ((env)->nvalues)
#define ecl_return0(env) \
        do { (env)->nvalues = 0; return ECL_NIL; } while (0)
#define ecl_return1(env,x) \
        do { (env)->nvalues = 1; return (x); } while (0)
#define ecl_return2(env,x,y)                                            \
        do {                                                            \
                cl_env_ptr __ecl_env = (env);                           \
                cl_object __aux1 = (x), __aux2=(y);                     \
                __ecl_env->values[1] = __aux2;                          \
                __ecl_env->nvalues = 2; return __aux1;                  \
        } while (0)
#define ecl_return3(env,x,y,z)                                          \
        do {                                                            \
                cl_env_ptr __ecl_env = (env);                           \
                cl_object __aux1=(x), __aux2=(y), __aux3=(z);           \
                __ecl_env->values[1] = __aux2;                          \
                __ecl_env->values[2] = __aux3;                          \
                __ecl_env->nvalues = 3; return __aux1;                  \
        } while (0)

/*************
 * LISP STACK
 *************/

#define ECL_VMS_REF(env,n) ((env)->vms_stack.top[n])

static inline void
ecl_vms_push(cl_env_ptr env, cl_object o) {
  cl_object *new_top = env->vms_stack.top;
  if (ecl_unlikely(new_top >= env->vms_stack.limit)) {
    new_top = ecl_vms_grow(env);
  }
  env->vms_stack.top = new_top+1;
  *new_top = (o);
}

static inline void
ecl_vms_push_n(cl_env_ptr env, cl_index n) {
  cl_object *new_top = env->vms_stack.top;
  while (ecl_unlikely((env->vms_stack.limit - new_top) <= n)) {
    new_top = ecl_vms_grow(env);
  }
  env->vms_stack.top = new_top + n;
}

static inline cl_object
ecl_vms_pop_unsafe(cl_env_ptr env)
{
  return *(--((env)->vms_stack.top));
}

static inline void
ecl_vms_pop_n_unsafe(cl_env_ptr env, cl_index n)
{
  env->vms_stack.top -= n;
}

static inline cl_index
ecl_vms_index(cl_env_ptr env) {
  return (env)->vms_stack.top - (env)->vms_stack.org;
}

static inline void
ecl_vms_unwind(cl_env_ptr env, cl_index ndx)
{
  env->vms_stack.top = env->vms_stack.org + (ndx);
}

#define ECL_STACK_FRAME_COPY(dest,orig) do {                            \
                cl_object __dest = (dest);                              \
                cl_object __orig = (orig);                              \
                cl_index __size = __orig->frame.size;                   \
                ecl_stack_frame_open(__orig->frame.env, __dest, __size); \
                memcpy(__dest->frame.base, __orig->frame.base, __size * sizeof(cl_object)); \
        } while (0);

#define ECL_STACK_FRAME_SET(f,ndx,o) do { (f)->frame.base[(ndx)] = (o); } while(0)
#define ECL_STACK_FRAME_REF(f,ndx) ((f)->frame.base[(ndx)])

/*********************************
 * HIGH LEVEL CONTROL STRUCTURES *
 *********************************/

#define ECL_UNWIND_PROTECT_BEGIN(the_env) do {     \
        bool __unwinding; ecl_frame_ptr __next_fr; \
        const cl_env_ptr __the_env = (the_env);    \
        cl_index __nr; \
        ecl_frs_push(__the_env,ECL_PROTECT_TAG);   \
        if (__ecl_frs_push_result) {      \
                __unwinding=1; __next_fr=__the_env->frs_stack.nlj_fr; \
        } else {

#define ECL_UNWIND_PROTECT_EXIT \
        __unwinding=0; } \
        ecl_frs_pop(__the_env); \
        __nr = ecl_vms_push_values(__the_env);

#define ECL_UNWIND_PROTECT_END \
        ecl_vms_pop_values(__the_env,__nr);   \
        if (__unwinding) ecl_unwind(__the_env,__next_fr); } while(0)

/* unwind-protect variant which disables interrupts during cleanup */
#define ECL_UNWIND_PROTECT_THREAD_SAFE_EXIT \
        __unwinding=0; } \
        ecl_bds_bind(__the_env,ECL_INTERRUPTS_ENABLED,ECL_NIL); \
        ecl_frs_pop(__the_env); \
        __nr = ecl_vms_push_values(__the_env);

#define ECL_UNWIND_PROTECT_THREAD_SAFE_END      \
        ecl_vms_pop_values(__the_env,__nr);   \
        ecl_bds_unwind1(__the_env); \
        ecl_check_pending_interrupts(__the_env); \
        if (__unwinding) ecl_unwind(__the_env,__next_fr); } while(0)

#define ECL_NEW_FRAME_ID(env) ecl_make_fixnum(env->frs_stack.frame_id++)

#define ECL_BLOCK_BEGIN(the_env,id) do {                        \
        const cl_object __id = ECL_NEW_FRAME_ID(the_env);       \
        const cl_env_ptr __the_env = (the_env);                 \
        ecl_frs_push(__the_env,__id);                           \
        if (__ecl_frs_push_result == 0)

#define ECL_BLOCK_END \
        ecl_frs_pop(__the_env); } while(0)

#define ECL_CATCH_BEGIN(the_env,tag) do {       \
        const cl_env_ptr __the_env = (the_env); \
        ecl_frs_push(__the_env,tag);            \
        if (__ecl_frs_push_result == 0) {

#define ECL_CATCH_END } \
        ecl_frs_pop(__the_env); } while (0)

#define ECL_RESTART_CASE_BEGIN(the_env, names) do {                     \
        const cl_env_ptr __the_env = (the_env);                         \
        const cl_object __ecl_tag = ecl_list1(names);                   \
        ecl_bds_bind(__the_env, ECL_RESTART_CLUSTERS,                   \
                     si_bind_simple_restarts(__ecl_tag, names));        \
        ecl_frs_push(__the_env,__ecl_tag);                              \
        if (__ecl_frs_push_result == 0) {

#define ECL_RESTART_CASE(code, args)                                    \
        } else if (__the_env->values[0] == ecl_make_fixnum(code)) {     \
        const cl_object args = __the_env->values[1];

#define ECL_RESTART_CASE_END }                  \
                ecl_frs_pop(__the_env);         \
                ecl_bds_unwind1(__the_env);     \
                } while (0)

#define ECL_HANDLER_CASE_BEGIN(the_env, names) do {                     \
        const cl_env_ptr __the_env = (the_env);                         \
        const cl_object __ecl_tag = ecl_list1(names);                   \
        ecl_bds_bind(__the_env, ECL_HANDLER_CLUSTERS,                   \
                     si_bind_simple_handlers(__ecl_tag, names));        \
        ecl_frs_push(__the_env,__ecl_tag);                              \
        if (__ecl_frs_push_result == 0) {

#define ECL_HANDLER_CASE(code, args)                                    \
        } else if (__the_env->values[0] == ecl_make_fixnum(code)) {     \
        const cl_object args = __the_env->values[1];

#define ECL_HANDLER_CASE_END }                  \
                ecl_frs_pop(__the_env);         \
                ecl_bds_unwind1(__the_env);     \
                } while (0)

#if defined(_MSC_VER)
# define ECL_CATCH_ALL_BEGIN(the_env) do {                      \
        const cl_env_ptr __the_env = (the_env);                 \
        _try {                                                  \
        const cl_env_ptr __the_env = (the_env);                 \
        ecl_frs_push(__the_env,ECL_PROTECT_TAG);                \
        if (__ecl_frs_push_result == 0) {
# define ECL_CATCH_ALL_IF_CAUGHT } else {
# define ECL_CATCH_ALL_END }}                                           \
        _except(_ecl_w32_exception_filter(GetExceptionInformation())) \
        { (void)0; }                                                    \
        ecl_frs_pop(__the_env); } while(0)
#else
# define ECL_CATCH_ALL_BEGIN(the_env) do {       \
        const cl_env_ptr __the_env = (the_env);  \
        ecl_frs_push(__the_env,ECL_PROTECT_TAG); \
        if (__ecl_frs_push_result == 0) {
# define ECL_CATCH_ALL_IF_CAUGHT } else {
# define ECL_CATCH_ALL_END } \
        ecl_frs_pop(__the_env); } while(0)
#endif


#ifdef __cplusplus
}
#endif

#endif /* ECL_STACKS_H */
