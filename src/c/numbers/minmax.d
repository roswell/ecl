/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * minmax.c - number sorting
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>

@(defun max (max &rest nums)
  @
  /* INV: type check occurs in ecl_number_compare() for the rest of
     numbers, but for the first argument it happens in ecl_zerop(). */
  if (narg-- == 1) {
    ecl_zerop(max);
  } else do {
      cl_object numi = ecl_va_arg(nums);
      if (ecl_number_compare(max, numi) < 0)
        max = numi;
    } while (--narg);
  @(return max);
  @)

@(defun min (min &rest nums)
  @
  /* INV: type check occurs in ecl_number_compare() for the rest of
     numbers, but for the first argument it happens in ecl_zerop(). */
  if (narg-- == 1) {
    ecl_zerop(min);
  } else do {
      cl_object numi = ecl_va_arg(nums);
      if (ecl_number_compare(min, numi) > 0)
        min = numi;
    } while (--narg);
  @(return min);
  @)
