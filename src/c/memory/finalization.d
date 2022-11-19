/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * finalization.c - object finalization
 *
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 * Copyright (c) 2022 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <ecl/threads.h>
#include <ecl/internal.h>

static void
standard_finalizer(cl_object o)
{
  switch (o->d.t) {
#ifdef ENABLE_DLOPEN
  case t_codeblock:
    ecl_library_close(o);
    break;
#endif
  case t_stream:
    cl_close(1, o);
    break;
  case t_weak_pointer:
    GC_unregister_disappearing_link((void**)&(o->weak.value));
    break;
#ifdef ECL_THREADS
  case t_lock: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->lock.mutex);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_condition_variable: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_cond_var_destroy(&o->condition_variable.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_barrier: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->barrier.mutex);
    ecl_cond_var_destroy(&o->barrier.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_semaphore: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->semaphore.mutex);
    ecl_cond_var_destroy(&o->semaphore.cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_mailbox: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->mailbox.mutex);
    ecl_cond_var_destroy(&o->mailbox.reader_cv);
    ecl_cond_var_destroy(&o->mailbox.writer_cv);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_rwlock: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_rwlock_destroy(&o->rwlock.mutex);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_process: {
    const cl_env_ptr the_env = ecl_process_env();
    ecl_disable_interrupts_env(the_env);
    ecl_mutex_destroy(&o->process.start_stop_lock);
    ecl_cond_var_destroy(&o->process.exit_barrier);
    ecl_enable_interrupts_env(the_env);
    break;
  }
  case t_symbol: {
    ecl_atomic_push(&cl_core.reused_indices,
                    ecl_make_fixnum(o->symbol.binding));
    o->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
  }
#endif /* ECL_THREADS */
  default:;
  }
}

static void
wrapped_finalizer(cl_object o, cl_object finalizer);

static void
register_finalizer(cl_object o, void *finalized_object,
                   GC_finalization_proc fn, void *cd,
                   GC_finalization_proc *ofn, void **ocd)
{
  /* Finalizers for some builtin objects are only run when the object is not
   * reachable by any means, including through other finalizers which might
   * make the object reachable again. The objects must not contain any cyclic
   * references for which finalizers are registered.
   *
   * We don't use this type of finalizer for user-defined finalizers, because
   * those might contain cyclic references which would prevent the objects
   * from being garbage collected. It is instead the duty of the user to write
   * the finalizers in a consistent way.
   *
   * case t_symbol: is not finalized with the "unreachable" finalizer because
   * it might contain cyclic references; Also running the finalizer too early
   * doesn't lead to any problems, we will simply choose a new binding index
   * the next time a binding is established. */
  switch (o->d.t) {
#ifdef ENABLE_DLOPEN
  case t_codeblock:
#endif
  case t_stream:
#if defined(ECL_THREADS)
  case t_lock:
  case t_condition_variable:
  case t_barrier:
  case t_semaphore:
  case t_mailbox:
  case t_rwlock:
  case t_process:
#endif  /* ECL_THREADS */
    /* Don't delete the standard finalizer. */
    if (fn == NULL) {
      fn = (GC_finalization_proc)wrapped_finalizer;
      cd = ECL_T;
    }
    GC_REGISTER_FINALIZER_UNREACHABLE(finalized_object, fn, cd, ofn, ocd);
    break;
  case t_weak_pointer:
#if defined(ECL_THREADS)
  case t_symbol:
#endif
    /* Don't delete the standard finalizer. */
    if (fn == NULL) {
      fn = (GC_finalization_proc)wrapped_finalizer;
      cd = ECL_T;
    }
    /* fallthrough */
  default:
    GC_REGISTER_FINALIZER_NO_ORDER(finalized_object, fn, cd, ofn, ocd);
    break;
  }
}

static void
deferred_finalizer(cl_object* x)
{
  wrapped_finalizer(x[0], x[1]);
}

static void
wrapped_finalizer(cl_object o, cl_object finalizer)
{
  if (finalizer != ECL_NIL && finalizer != NULL) {
#ifdef ECL_THREADS
    const cl_env_ptr the_env = ecl_process_env_unsafe();
    if (!the_env
        || !the_env->own_process
        || the_env->own_process->process.phase < ECL_PROCESS_ACTIVE)
    {
       /*
        * The finalizer is invoked while we are registering or setup a
        * new lisp process.  As example that may happen when we are
        * doing ecl_import_current_thread.  That mean the finalizer
        * can not be executed right now, so in some way we need to
        * queue the finalization.  When we return from this function
        * the original finalizer is no more registered to o, and if o
        * is not anymore reachable it will be colleted.  To prevent
        * this we need to make this object reachable again after that
        * roundtrip and postpone the finalization to the next garbage
        * collection.  Given that this is a rare condition one way to
        * do that is:
        */
       GC_finalization_proc ofn;
       void *odata;
       cl_object* wrapper = GC_MALLOC(2*sizeof(cl_object));
       wrapper[0] = o;
       wrapper[1] = finalizer;

       register_finalizer(o, wrapper,
                          (GC_finalization_proc)deferred_finalizer, 0,
                          &ofn, &odata);
       return;
    }
#endif /* ECL_THREADS */
    CL_NEWENV_BEGIN {
      if (finalizer != ECL_T) {
        funcall(2, finalizer, o);
      }
      standard_finalizer(o);
    } CL_NEWENV_END;
  }
}

cl_object
si_get_finalizer(cl_object o)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object output;
  GC_finalization_proc ofn;
  void *odata;
  ecl_disable_interrupts_env(the_env);
  register_finalizer(o, o, (GC_finalization_proc)0, 0, &ofn, &odata);
  if (ofn == 0) {
    output = ECL_NIL;
  } else if (ofn == (GC_finalization_proc)wrapped_finalizer) {
    output = (cl_object)odata;
  } else {
    output = ECL_NIL;
  }
  register_finalizer(o, o, ofn, odata, &ofn, &odata);
  ecl_enable_interrupts_env(the_env);
  @(return output);
}

void
ecl_set_finalizer_unprotected(cl_object o, cl_object finalizer)
{
  GC_finalization_proc ofn;
  void *odata;
  if (finalizer == ECL_NIL) {
    register_finalizer(o, o, (GC_finalization_proc)0, 0, &ofn, &odata);
  } else {
    GC_finalization_proc newfn;
    newfn = (GC_finalization_proc)wrapped_finalizer;
    register_finalizer(o, o, newfn, finalizer, &ofn, &odata);
  }
}

cl_object
si_set_finalizer(cl_object o, cl_object finalizer)
{
  const cl_env_ptr the_env = ecl_process_env();
  ecl_disable_interrupts_env(the_env);
  ecl_set_finalizer_unprotected(o, finalizer);
  ecl_enable_interrupts_env(the_env);
  @(return);
}
