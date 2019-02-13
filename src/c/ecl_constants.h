/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * ecl_constants.h - contstant values for all_symbols.d
 *
 * Copyright (c) 2010 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <float.h>
#include <ecl/ecl-inl.h>

ecl_def_ct_single_float(flt_max,FLT_MAX,static,const);
ecl_def_ct_single_float(flt_max_neg,-FLT_MAX,static,const);
ecl_def_ct_single_float(flt_min,FLT_TRUE_MIN,static,const);
ecl_def_ct_single_float(flt_min_neg,-FLT_TRUE_MIN,static,const);
ecl_def_ct_single_float(flt_min_norm,FLT_MIN,static,const);
ecl_def_ct_single_float(flt_min_neg_norm,-FLT_MIN,static,const);

#define ECL_LEAST_POSITIVE_SINGLE_FLOAT (cl_object)(&flt_min_data)
#define ECL_LEAST_NEGATIVE_SINGLE_FLOAT (cl_object)(&flt_min_neg_data)
#define ECL_LEAST_POSITIVE_NORMALIZED_SINGLE_FLOAT (cl_object)(&flt_min_norm_data)
#define ECL_LEAST_NEGATIVE_NORMALIZED_SINGLE_FLOAT (cl_object)(&flt_min_neg_norm_data)
#define ECL_MOST_POSITIVE_SINGLE_FLOAT (cl_object)(&flt_max_data)
#define ECL_MOST_NEGATIVE_SINGLE_FLOAT (cl_object)(&flt_max_neg_data)

#define ECL_LEAST_POSITIVE_SHORT_FLOAT (cl_object)(&flt_min_data)
#define ECL_LEAST_NEGATIVE_SHORT_FLOAT (cl_object)(&flt_min_neg_data)
#define ECL_LEAST_POSITIVE_NORMALIZED_SHORT_FLOAT (cl_object)(&flt_min_norm_data)
#define ECL_LEAST_NEGATIVE_NORMALIZED_SHORT_FLOAT (cl_object)(&flt_min_neg_norm_data)
#define ECL_MOST_POSITIVE_SHORT_FLOAT (cl_object)(&flt_max_data)
#define ECL_MOST_NEGATIVE_SHORT_FLOAT (cl_object)(&flt_max_neg_data)

ecl_def_ct_double_float(dbl_max,DBL_MAX,static,const);
ecl_def_ct_double_float(dbl_max_neg,-DBL_MAX,static,const);
ecl_def_ct_double_float(dbl_min,DBL_TRUE_MIN,static,const);
ecl_def_ct_double_float(dbl_min_neg,-DBL_TRUE_MIN,static,const);
ecl_def_ct_double_float(dbl_min_norm,DBL_MIN,static,const);
ecl_def_ct_double_float(dbl_min_neg_norm,-DBL_MIN,static,const);

#define ECL_LEAST_POSITIVE_DOUBLE_FLOAT (cl_object)(&dbl_min_data)
#define ECL_LEAST_NEGATIVE_DOUBLE_FLOAT (cl_object)(&dbl_min_neg_data)
#define ECL_LEAST_POSITIVE_NORMALIZED_DOUBLE_FLOAT (cl_object)(&dbl_min_norm_data)
#define ECL_LEAST_NEGATIVE_NORMALIZED_DOUBLE_FLOAT (cl_object)(&dbl_min_neg_norm_data)
#define ECL_MOST_POSITIVE_DOUBLE_FLOAT (cl_object)(&dbl_max_data)
#define ECL_MOST_NEGATIVE_DOUBLE_FLOAT (cl_object)(&dbl_max_neg_data)

#ifdef ECL_LONG_FLOAT
ecl_def_ct_long_float(ldbl_max,LDBL_MAX,static,const);
ecl_def_ct_long_float(ldbl_max_neg,-LDBL_MAX,static,const);
ecl_def_ct_long_float(ldbl_min,LDBL_TRUE_MIN,static,const);
ecl_def_ct_long_float(ldbl_min_neg,-LDBL_TRUE_MIN,static,const);
ecl_def_ct_long_float(ldbl_min_norm,LDBL_MIN,static,const);
ecl_def_ct_long_float(ldbl_min_neg_norm,-LDBL_MIN,static,const);
#define ECL_LEAST_POSITIVE_LONG_FLOAT (cl_object)(&ldbl_min_data)
#define ECL_LEAST_NEGATIVE_LONG_FLOAT (cl_object)(&ldbl_min_neg_data)
#define ECL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT (cl_object)(&ldbl_min_norm_data)
#define ECL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT (cl_object)(&ldbl_min_neg_norm_data)
#define ECL_MOST_POSITIVE_LONG_FLOAT (cl_object)(&ldbl_max_data)
#define ECL_MOST_NEGATIVE_LONG_FLOAT (cl_object)(&ldbl_max_neg_data)
#else
#define ECL_LEAST_POSITIVE_LONG_FLOAT (cl_object)(&dbl_min_data)
#define ECL_LEAST_NEGATIVE_LONG_FLOAT (cl_object)(&dbl_min_neg_data)
#define ECL_LEAST_POSITIVE_NORMALIZED_LONG_FLOAT (cl_object)(&dbl_min_norm_data)
#define ECL_LEAST_NEGATIVE_NORMALIZED_LONG_FLOAT (cl_object)(&dbl_min_neg_norm_data)
#define ECL_MOST_POSITIVE_LONG_FLOAT (cl_object)(&dbl_max_data)
#define ECL_MOST_NEGATIVE_LONG_FLOAT (cl_object)(&dbl_max_neg_data)
#endif

#ifdef ECL_LONG_FLOAT
ecl_def_ct_long_float(float_pi,ECL_PI_L,static,const);
#else
ecl_def_ct_double_float(float_pi,ECL_PI_D,static,const);
#endif
#define ECL_PI (cl_object)(&float_pi_data)
