/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * ecnum_comp.c - comparisons on numbers
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/*
 * In Common Lisp, comparisons between floats and integers are performed
 * via an intermediate rationalization of the floating point number. In C,
 * on the other hand, the comparison is performed by converting the integer
 * into a floating point number. However, if the double type is too small
 * this may lead to a loss of precision and two numbers being told equal
 * when, by Common Lisp standards, would not.
 */
static int
double_fix_compare(cl_fixnum n, double d)
{
  if ((double)n < d) {
    return -1;
  } else if ((double)n > d) {
    return +1;
  } else if (sizeof(double) > sizeof(cl_fixnum)) {
    return 0;
  } else {
    /* When we reach here, the double type has no
     * significant decimal part. However, as explained
     * above, the double type is too small and integers
     * may coerce to the same double number giving a false
     * positive. Hence we perform the comparison in
     * integer space. */
    cl_fixnum m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}

static int
long_double_fix_compare(cl_fixnum n, long double d)
{
  if ((long double)n < d) {
    return -1;
  } else if ((long double)n > d) {
    return +1;
  } else if (sizeof(long double) > sizeof(cl_fixnum)) {
    return 0;
  } else {
    cl_fixnum m = d;
    if (n == m) {
      return 0;
    } else if (n > m) {
      return +1;
    } else {
      return -1;
    }
  }
}

