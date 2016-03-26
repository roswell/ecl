/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
    cfun_dispatch.c -- Trampolines for functions

    Note:
      Unified argument passing in dispatch_variadic_N() is fast, correct,
    and consumes less stack space. Using switch clause to pass exact number
    of arguments would result dozens of call sites, consuming much more stack
    space per call. It's the same reason for pre-dispatching on narg_fixed.

*/


#if ECL_MULTIPLE_VALUES_LIMIT != 64
#error ECL_MULTIPLE_VALUES_LIMIT != 64, which is presumed in dispatch_fixed/dispatch_variadic
#endif


typedef cl_object (*cfun_fixed_0) ();
typedef cl_object (*cfun_fixed_1) (cl_object);
typedef cl_object (*cfun_fixed_2) (cl_object, cl_object);
typedef cl_object (*cfun_fixed_3) (cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_4) (cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_5) (cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_6) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_7) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_8) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_9) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_10) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_11) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_12) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_13) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_14) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_15) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_16) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_17) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_18) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_19) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_20) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_21) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_22) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_23) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_24) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_25) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_26) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_27) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_28) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_29) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_30) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_31) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_32) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_33) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_34) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_35) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_36) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_37) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_38) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_39) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_40) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_41) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_42) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_43) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_44) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_45) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_46) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_47) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_48) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_49) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_50) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_51) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_52) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_53) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_54) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_55) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_56) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_57) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_58) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_59) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_60) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_61) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_62) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cfun_fixed_63) (cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);


typedef cl_object (*cfun_variadic_0) (cl_narg, ...);
typedef cl_object (*cfun_variadic_1) (cl_narg, cl_object, ...);
typedef cl_object (*cfun_variadic_2) (cl_narg, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_3) (cl_narg, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_4) (cl_narg, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_5) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_6) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_7) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_8) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_9) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_10) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_11) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_12) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_13) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_14) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_15) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_16) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_17) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_18) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_19) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_20) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_21) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_22) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_23) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_24) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_25) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_26) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_27) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_28) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_29) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_30) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_31) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_32) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_33) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_34) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_35) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_36) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_37) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_38) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_39) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_40) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_41) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_42) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_43) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_44) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_45) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_46) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_47) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_48) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_49) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_50) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_51) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_52) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_53) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_54) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_55) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_56) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_57) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_58) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_59) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_60) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_61) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_62) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cfun_variadic_63) (cl_narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);


static cl_object dispatch_fixed_0(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_0: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 0 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_0)cfunptr)();
}

static cl_object dispatch_fixed_1(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_1: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 1 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_1)cfunptr)(v[0]);
}

static cl_object dispatch_fixed_2(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_2: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 2 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_2)cfunptr)(v[0], v[1]);
}

static cl_object dispatch_fixed_3(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_3: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 3 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_3)cfunptr)(v[0], v[1], v[2]);
}

static cl_object dispatch_fixed_4(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_4: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 4 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_4)cfunptr)(v[0], v[1], v[2], v[3]);
}

static cl_object dispatch_fixed_5(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_5: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 5 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_5)cfunptr)(v[0], v[1], v[2], v[3], v[4]);
}

static cl_object dispatch_fixed_6(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_6: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 6 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_6)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5]);
}

static cl_object dispatch_fixed_7(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_7: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 7 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_7)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
}

static cl_object dispatch_fixed_8(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_8: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 8 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_8)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
}

static cl_object dispatch_fixed_9(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_9: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 9 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_9)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
}

static cl_object dispatch_fixed_10(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_10: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 10 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_10)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
}

static cl_object dispatch_fixed_11(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_11: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 11 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_11)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10]);
}

static cl_object dispatch_fixed_12(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_12: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 12 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_12)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11]);
}

static cl_object dispatch_fixed_13(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_13: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 13 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_13)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12]);
}

static cl_object dispatch_fixed_14(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_14: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 14 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_14)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13]);
}

static cl_object dispatch_fixed_15(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_15: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 15 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_15)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14]);
}

static cl_object dispatch_fixed_16(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_16: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 16 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_16)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15]);
}

static cl_object dispatch_fixed_17(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_17: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 17 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_17)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16]);
}

static cl_object dispatch_fixed_18(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_18: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 18 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_18)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17]);
}

static cl_object dispatch_fixed_19(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_19: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 19 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_19)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18]);
}

static cl_object dispatch_fixed_20(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_20: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 20 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_20)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19]);
}

static cl_object dispatch_fixed_21(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_21: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 21 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_21)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20]);
}

static cl_object dispatch_fixed_22(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_22: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 22 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_22)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21]);
}

static cl_object dispatch_fixed_23(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_23: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 23 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_23)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22]);
}

static cl_object dispatch_fixed_24(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_24: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 24 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_24)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23]);
}

static cl_object dispatch_fixed_25(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_25: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 25 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_25)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24]);
}

static cl_object dispatch_fixed_26(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_26: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 26 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_26)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25]);
}

static cl_object dispatch_fixed_27(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_27: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 27 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_27)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26]);
}

static cl_object dispatch_fixed_28(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_28: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 28 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_28)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27]);
}

static cl_object dispatch_fixed_29(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_29: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 29 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_29)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28]);
}

static cl_object dispatch_fixed_30(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_30: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 30 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_30)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29]);
}

static cl_object dispatch_fixed_31(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_31: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 31 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_31)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30]);
}

static cl_object dispatch_fixed_32(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_32: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 32 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_32)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31]);
}

static cl_object dispatch_fixed_33(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_33: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 33 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_33)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32]);
}

static cl_object dispatch_fixed_34(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_34: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 34 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_34)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33]);
}

static cl_object dispatch_fixed_35(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_35: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 35 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_35)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34]);
}

static cl_object dispatch_fixed_36(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_36: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 36 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_36)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35]);
}

static cl_object dispatch_fixed_37(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_37: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 37 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_37)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36]);
}

static cl_object dispatch_fixed_38(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_38: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 38 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_38)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37]);
}

static cl_object dispatch_fixed_39(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_39: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 39 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_39)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38]);
}

static cl_object dispatch_fixed_40(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_40: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 40 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_40)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39]);
}

static cl_object dispatch_fixed_41(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_41: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 41 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_41)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40]);
}

static cl_object dispatch_fixed_42(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_42: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 42 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_42)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41]);
}

static cl_object dispatch_fixed_43(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_43: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 43 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_43)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42]);
}

static cl_object dispatch_fixed_44(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_44: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 44 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_44)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43]);
}

static cl_object dispatch_fixed_45(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_45: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 45 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_45)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44]);
}

static cl_object dispatch_fixed_46(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_46: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 46 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_46)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45]);
}

static cl_object dispatch_fixed_47(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_47: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 47 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_47)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46]);
}

static cl_object dispatch_fixed_48(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_48: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 48 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_48)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47]);
}

static cl_object dispatch_fixed_49(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_49: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 49 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_49)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48]);
}

static cl_object dispatch_fixed_50(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_50: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 50 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_50)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49]);
}

static cl_object dispatch_fixed_51(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_51: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 51 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_51)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50]);
}

static cl_object dispatch_fixed_52(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_52: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 52 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_52)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51]);
}

static cl_object dispatch_fixed_53(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_53: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 53 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_53)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52]);
}

static cl_object dispatch_fixed_54(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_54: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 54 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_54)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53]);
}

static cl_object dispatch_fixed_55(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_55: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 55 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_55)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54]);
}

static cl_object dispatch_fixed_56(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_56: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 56 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_56)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55]);
}

static cl_object dispatch_fixed_57(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_57: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 57 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_57)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56]);
}

static cl_object dispatch_fixed_58(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_58: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 58 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_58)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57]);
}

static cl_object dispatch_fixed_59(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_59: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 59 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_59)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58]);
}

static cl_object dispatch_fixed_60(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_60: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 60 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_60)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59]);
}

static cl_object dispatch_fixed_61(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_61: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 61 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_61)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60]);
}

static cl_object dispatch_fixed_62(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_62: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 62 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_62)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61]);
}

static cl_object dispatch_fixed_63(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfunfixed.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfunfixed.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfunfixed) FEerror("Bad function for cfun_dispath_fixed_63: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 63 || minarg != narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);

  return ((cfun_fixed_63)cfunptr)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62]);
}


static cl_object dispatch_variadic_0(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_0: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 0 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_0)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_1(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_1: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 1 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_1)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_2(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_2: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 2 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_2)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_3(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_3: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 3 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_3)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_4(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_4: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 4 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_4)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_5(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_5: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 5 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_5)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_6(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_6: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 6 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_6)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_7(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_7: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 7 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_7)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_8(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_8: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 8 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_8)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_9(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_9: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 9 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_9)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_10(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_10: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 10 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_10)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_11(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_11: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 11 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_11)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_12(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_12: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 12 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_12)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_13(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_13: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 13 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_13)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_14(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_14: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 14 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_14)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_15(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_15: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 15 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_15)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_16(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_16: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 16 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_16)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_17(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_17: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 17 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_17)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_18(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_18: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 18 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_18)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_19(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_19: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 19 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_19)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_20(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_20: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 20 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_20)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_21(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_21: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 21 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_21)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_22(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_22: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 22 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_22)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_23(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_23: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 23 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_23)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_24(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_24: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 24 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_24)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_25(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_25: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 25 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_25)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_26(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_26: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 26 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_26)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_27(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_27: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 27 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_27)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_28(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_28: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 28 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_28)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_29(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_29: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 29 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_29)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_30(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_30: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 30 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_30)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_31(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_31: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 31 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_31)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_32(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_32: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 32 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_32)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_33(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_33: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 33 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_33)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_34(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_34: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 34 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_34)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_35(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_35: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 35 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_35)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_36(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_36: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 36 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_36)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_37(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_37: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 37 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_37)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_38(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_38: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 38 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_38)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_39(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_39: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 39 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_39)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_40(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_40: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 40 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_40)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_41(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_41: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 41 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_41)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_42(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_42: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 42 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_42)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_43(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_43: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 43 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_43)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_44(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_44: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 44 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_44)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_45(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_45: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 45 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_45)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_46(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_46: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 46 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_46)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_47(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_47: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 47 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_47)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_48(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_48: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 48 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_48)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_49(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_49: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 49 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_49)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_50(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_50: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 50 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_50)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_51(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_51: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 51 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_51)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_52(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_52: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 52 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_52)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_53(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_53: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 53 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_53)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_54(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_54: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 54 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_54)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_55(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_55: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 55 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_55)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_56(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_56: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 56 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_56)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_57(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_57: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 57 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_57)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_58(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_58: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 58 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_58)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_59(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_59: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 59 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_59)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_60(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_60: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 60 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_60)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_61(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_61: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 61 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_61)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_62(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_62: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 62 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_62)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_object dispatch_variadic_63(cl_narg narg, ...) {
  ecl_va_list args;
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  cl_cfunptr cfunptr = fun->cfun.cfunptr;
  cl_type type = ecl_t_of(fun);
  cl_narg minarg = fun->cfun.narg_fixed;
  cl_object v[ECL_MULTIPLE_VALUES_LIMIT];
  int i = 0, n = narg < ECL_MULTIPLE_VALUES_LIMIT ? narg : ECL_MULTIPLE_VALUES_LIMIT;

  if (type != t_cfun && type != t_cclosure) FEerror("Bad function for cfun_dispath_variadic_63: ~A.", 1, fun);
  if (ecl_unlikely(minarg != 63 || minarg > narg)) FEwrong_num_arguments(fun);

  ecl_va_start(args, narg, narg, 0);
  for (i = 0; i < n; i++)
    v[i] = ecl_va_arg(args);
  ecl_va_end(args);
  /* Arguments above ECL_MULTIPLE_VALUES_LIMIT have been pushed on the stack */

  return ((cfun_variadic_63)cfunptr)(narg, v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15], v[16], v[17], v[18], v[19], v[20], v[21], v[22], v[23], v[24], v[25], v[26], v[27], v[28], v[29], v[30], v[31], v[32], v[33], v[34], v[35], v[36], v[37], v[38], v[39], v[40], v[41], v[42], v[43], v[44], v[45], v[46], v[47], v[48], v[49], v[50], v[51], v[52], v[53], v[54], v[55], v[56], v[57], v[58], v[59], v[60], v[61], v[62], v[63]);
}

static cl_objectfn cfun_fixed_dispatch_table[64] = {
  dispatch_fixed_0,
  dispatch_fixed_1,
  dispatch_fixed_2,
  dispatch_fixed_3,
  dispatch_fixed_4,
  dispatch_fixed_5,
  dispatch_fixed_6,
  dispatch_fixed_7,
  dispatch_fixed_8,
  dispatch_fixed_9,
  dispatch_fixed_10,
  dispatch_fixed_11,
  dispatch_fixed_12,
  dispatch_fixed_13,
  dispatch_fixed_14,
  dispatch_fixed_15,
  dispatch_fixed_16,
  dispatch_fixed_17,
  dispatch_fixed_18,
  dispatch_fixed_19,
  dispatch_fixed_20,
  dispatch_fixed_21,
  dispatch_fixed_22,
  dispatch_fixed_23,
  dispatch_fixed_24,
  dispatch_fixed_25,
  dispatch_fixed_26,
  dispatch_fixed_27,
  dispatch_fixed_28,
  dispatch_fixed_29,
  dispatch_fixed_30,
  dispatch_fixed_31,
  dispatch_fixed_32,
  dispatch_fixed_33,
  dispatch_fixed_34,
  dispatch_fixed_35,
  dispatch_fixed_36,
  dispatch_fixed_37,
  dispatch_fixed_38,
  dispatch_fixed_39,
  dispatch_fixed_40,
  dispatch_fixed_41,
  dispatch_fixed_42,
  dispatch_fixed_43,
  dispatch_fixed_44,
  dispatch_fixed_45,
  dispatch_fixed_46,
  dispatch_fixed_47,
  dispatch_fixed_48,
  dispatch_fixed_49,
  dispatch_fixed_50,
  dispatch_fixed_51,
  dispatch_fixed_52,
  dispatch_fixed_53,
  dispatch_fixed_54,
  dispatch_fixed_55,
  dispatch_fixed_56,
  dispatch_fixed_57,
  dispatch_fixed_58,
  dispatch_fixed_59,
  dispatch_fixed_60,
  dispatch_fixed_61,
  dispatch_fixed_62,
  dispatch_fixed_63
};


static cl_objectfn cfun_variadic_dispatch_table[64] = {
  dispatch_variadic_0,
  dispatch_variadic_1,
  dispatch_variadic_2,
  dispatch_variadic_3,
  dispatch_variadic_4,
  dispatch_variadic_5,
  dispatch_variadic_6,
  dispatch_variadic_7,
  dispatch_variadic_8,
  dispatch_variadic_9,
  dispatch_variadic_10,
  dispatch_variadic_11,
  dispatch_variadic_12,
  dispatch_variadic_13,
  dispatch_variadic_14,
  dispatch_variadic_15,
  dispatch_variadic_16,
  dispatch_variadic_17,
  dispatch_variadic_18,
  dispatch_variadic_19,
  dispatch_variadic_20,
  dispatch_variadic_21,
  dispatch_variadic_22,
  dispatch_variadic_23,
  dispatch_variadic_24,
  dispatch_variadic_25,
  dispatch_variadic_26,
  dispatch_variadic_27,
  dispatch_variadic_28,
  dispatch_variadic_29,
  dispatch_variadic_30,
  dispatch_variadic_31,
  dispatch_variadic_32,
  dispatch_variadic_33,
  dispatch_variadic_34,
  dispatch_variadic_35,
  dispatch_variadic_36,
  dispatch_variadic_37,
  dispatch_variadic_38,
  dispatch_variadic_39,
  dispatch_variadic_40,
  dispatch_variadic_41,
  dispatch_variadic_42,
  dispatch_variadic_43,
  dispatch_variadic_44,
  dispatch_variadic_45,
  dispatch_variadic_46,
  dispatch_variadic_47,
  dispatch_variadic_48,
  dispatch_variadic_49,
  dispatch_variadic_50,
  dispatch_variadic_51,
  dispatch_variadic_52,
  dispatch_variadic_53,
  dispatch_variadic_54,
  dispatch_variadic_55,
  dispatch_variadic_56,
  dispatch_variadic_57,
  dispatch_variadic_58,
  dispatch_variadic_59,
  dispatch_variadic_60,
  dispatch_variadic_61,
  dispatch_variadic_62,
  dispatch_variadic_63
};
