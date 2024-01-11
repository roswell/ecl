/* -*- Mode: C; c-basic-offset: 8; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=8 shiftwidth=4 expandtab: */

/*
 * fpe_none.c -- Nonportable component of the floating point code (dummy)
 *
 * Copyright (c) 2005, Juan Jose Garcia Ripoll.
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/*
 * The ecl_detect_fpe() is a macro (or a function) that detects whether a
 * floating point exception has been produced in a recent time. Currently
 * it is of great importance in the x86 architecture because exceptions
 * are not signaled on the instruction that originates the problem, but on
 * the next FP instruction. However, we could conceivably write a more
 * complex code on architectures that do not signal FP exceptions by
 * checking the control word of the FPU and doing the dispatching ourselves
 * instead of using signal().
 */

#define ecl_detect_fpe() (void)0
