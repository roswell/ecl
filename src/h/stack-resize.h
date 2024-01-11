/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */


/*
 * Copyright (c) 2001, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* internal.h -- safe stack resizing */

#ifndef ECL_STACK_RESIZE_H
#define ECL_STACK_RESIZE_H

/* We can't block interrupts with ecl_disable_interrupts() and write
 * in the thread local environment if we use fast interrupt dispatch
 * via mprotect(), so we have to use sigprocmask instead. No
 * performance problems, since this is only used for stack
 * resizing. */
#if defined(ECL_THREADS) && defined(ECL_USE_MPROTECT)
# ifdef HAVE_SIGPROCMASK
#  include <signal.h>
#  define ECL_STACK_RESIZE_DISABLE_INTERRUPTS(the_env) \
        sigset_t __sigset_new, __sigset_previous;      \
        sigfillset(&__sigset_new);                     \
        sigdelset(&__sigset_new, SIGSEGV);             \
        sigdelset(&__sigset_new, SIGBUS);              \
        pthread_sigmask(SIG_BLOCK, &__sigset_new, &__sigset_previous)
#  define ECL_STACK_RESIZE_ENABLE_INTERRUPTS(the_env) \
        pthread_sigmask(SIG_SETMASK, &__sigset_previous, NULL)
# else
#  error "Can't protect stack resizing from interrupts without sigprocmask. Either build ECL without mprotect() or live with possible race conditions."
# endif  /* HAVE_SIGPROCMASK */
#else
# define ECL_STACK_RESIZE_DISABLE_INTERRUPTS(the_env) ecl_disable_interrupts_env(the_env);
# define ECL_STACK_RESIZE_ENABLE_INTERRUPTS(the_env) ecl_enable_interrupts_env(env);
# endif  /* ECL_THREADS && ECL_USE_MPROTECT */

#endif  /* ECL_STACK_RESIZE_H */
