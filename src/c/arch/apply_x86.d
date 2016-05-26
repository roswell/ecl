/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * apply.c - interface to C call mechanism (x86 specific)
 *
 * Copyright (c) 2008 Giuseppe Attardi
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>

cl_object
APPLY(cl_narg n, cl_objectfn fn, cl_object *x)
{
  cl_object output;
  asm volatile (
                "movl   4(%%ebp),%%edx\n\t"     /* Create a fake frame for debugger */
                "pushl  %%edx\n\t"
                "pushl  %%ebp\n\t"
                "movl   %%ecx, %%edx\n\t"
                "cmpl   $63, %%ecx\n\t"         /* Copy at most 63 arguments onto the stack */
                "jle    FOO1\n\t"
                "movl   $63, %%ecx\n\t"
"FOO1:\n\t"                                     /* Here we compute the new address of the stack pointer */
                "movl   %%esp, %%ebp\n\t"       /* using the formula ESP = (ESP - ECX*4 - 4) & -16 */
                "negl   %%ecx\n\t"              /* which rounds ESP making it a multiple of 16 bytes. */
                "leal   -4(%%esp,%%ecx,4), %%esp\n\t"
                "andl   $-16, %%esp\n\t"
                "movl   %%edx, (%%esp)\n\t"     /* Then ESP[0] is the number of arguments */
                "negl   %%ecx\n\t"
                "leal   4(%%esp), %%edi\n\t"    /* and the other arguments are copied from ESP[4] on */
                "rep\n\t"
                "movsl\n\t"
                "call   *%%eax\n\t"             /* At this point the stack must be aligned */
                "movl   %%ebp, %%esp\n\t"
                "popl   %%ebp\n\t"
                "popl   %%edx\n\t"
                : "=a" (output) : "c" (n), "a" (fn), "S" (x) : "%edx", "%edi");
  return output;
}

cl_object
APPLY_fixed(cl_narg n, cl_object (*fn)(), cl_object *x)
{
  cl_object output;
  asm volatile (
                "movl   4(%%ebp),%%edx\n\t"     /* Create a fake frame for debugger */
                "pushl  %%edx\n\t"
                "pushl  %%ebp\n\t"
                "movl   %%ecx, %%edx\n\t"       /* Copy at most 63 arguments onto the stack */
                "cmpl   $63, %%ecx\n\t"
                "jle    FOO2\n\t"
                "movl   $63, %%ecx\n"
"FOO2:\n\t"                                     /* Here we compute the new address of the stack pointer */
                "movl   %%esp, %%ebp\n\t"       /* using the formula ESP = (ESP - ECX*4) & -16 */
                "negl   %%ecx\n\t"              /* which rounds ESP making it a multiple of 16 bytes. */
                "leal   (%%esp,%%ecx,4), %%esp\n\t"
                "andl   $-16, %%esp\n\t"
                "negl   %%ecx\n\t"
                "movl   %%esp, %%edi\n\t"       /* then the arguments are copied from ESP[0] on */
                "rep\n\t"
                "movsl\n\t"
                "call   *%%eax\n\t"             /* At this point the stack must be aligned */
                "movl   %%ebp, %%esp\n\t"
                "popl   %%ebp\n\t"
                "popl   %%edx\n\t"
                : "=a" (output) : "c" (n), "a" (fn), "S" (x) : "%edx", "%edi");
  return output;
}

cl_object
APPLY_closure(cl_narg n, cl_objectfn fn, cl_object cl, cl_object *x)
{
  cl_object output;
  asm volatile (
                "movl   4(%%ebp),%%edx\n\t"     /* Create a fake frame for debugger */
                "pushl  %%edx\n\t"
                "pushl  %%ebp\n\t"
                "movl   %%ecx, %%edx\n\t"
                "cmpl   $63, %%ecx\n\t"         /* Copy at most 63 arguments onto the stack */
                "jle    FOO3\n\t"
                "movl   $63, %%ecx\n\t"
"FOO3:\n\t"                                     /* Here we compute the new address of the stack pointer */
                "movl   %%esp, %%ebp\n\t"       /* using the formula ESP = (ESP - ECX*4 - 8) & -16 */
                "negl   %%ecx\n\t"              /* which rounds ESP making it a multiple of 16 bytes. */
                "leal   -8(%%esp,%%ecx,4), %%esp\n\t"
                "andl   $-16, %%esp\n\t"
                "movl   %%edx, (%%esp)\n\t"     /* Then ESP[0] is the number of arguments */
                "movl   %%edi, 4(%%esp)\n\t"    /* ESP[4] is the closure environment */
                "negl   %%ecx\n\t"
                "leal   8(%%esp), %%edi\n\t"    /* and the other arguments are copied from ESP[8] on */
                "rep\n\t"
                "movsl\n\t"
                "call   *%%eax\n\t"             /* At this point the stack must be aligned */
                "movl   %%ebp, %%esp\n\t"
                "popl   %%ebp\n\t"
                "popl   %%edx\n\t"
                : "=a" (output) : "c" (n), "a" (fn), "S" (x), "D" (cl) : "%edx");
  return output;
}
