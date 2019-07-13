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
     numbers, but for an unary argument it happens here. */
  if (narg-- == 1) {
    if (! ECL_REAL_TYPE_P(ecl_t_of(max))) {
      FEwrong_type_nth_arg(@[max], 1, max, @[real]);
    }
  } else do {
      cl_object numi = ecl_va_arg(nums);
      if (ecl_lower(max, numi)
#ifdef ECL_IEEE_FP
          || ecl_float_nan_p(max)
#endif
          )
        max = numi;
    } while (--narg);
  @(return max);
  @)

@(defun min (min &rest nums)
  @
  /* INV: type check occurs in ecl_number_compare() for the rest of
     numbers, but for an unary argument it happens here. */
  if (narg-- == 1) {
    if (! ECL_REAL_TYPE_P(ecl_t_of(min))) {
      FEwrong_type_nth_arg(@[min], 1, min, @[real]);
    }
  } else do {
      cl_object numi = ecl_va_arg(nums);
      if (ecl_greater(min, numi)
#ifdef ECL_IEEE_FP
          || ecl_float_nan_p(min)
#endif
          )
        min = numi;
    } while (--narg);
  @(return min);
  @)
