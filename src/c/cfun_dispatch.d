/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
  cfun_dispatch.c - trampolines for functions
*/

#if !(ECL_C_ARGUMENTS_LIMIT == 63)
#error "Please adjust code to the constant!"
#endif

typedef cl_object (*cl_objectfn_fixed0)();
typedef cl_object (*cl_objectfn_fixed1)(cl_object);
typedef cl_object (*cl_objectfn_fixed2)(cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed3)(cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed4)(cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed5)(cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed6)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed7)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed8)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed9)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed10)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed11)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed12)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed13)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed14)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed15)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed16)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed17)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed18)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed19)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed20)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed21)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed22)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed23)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed24)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed25)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed26)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed27)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed28)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed29)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed30)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed31)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed32)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed33)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed34)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed35)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed36)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed37)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed38)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed39)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed40)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed41)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed42)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed43)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed44)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed45)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed46)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed47)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed48)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed49)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed50)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed51)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed52)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed53)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed54)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed55)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed56)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed57)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed58)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed59)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed60)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed61)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed62)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);
typedef cl_object (*cl_objectfn_fixed63)(cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object);

static cl_object fixed_dispatch0 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  if (ecl_unlikely(narg != 0)) FEwrong_num_arguments(fun);
  return ((cl_objectfn_fixed0) fun->cfunfixed.entry_fixed)();
}

static cl_object fixed_dispatch1 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[1];
  if (ecl_unlikely(narg != 1)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 1; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed1) fun->cfunfixed.entry_fixed)(x[0]);
}

static cl_object fixed_dispatch2 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[2];
  if (ecl_unlikely(narg != 2)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 2; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed2) fun->cfunfixed.entry_fixed)(x[0], x[1]);
}

static cl_object fixed_dispatch3 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[3];
  if (ecl_unlikely(narg != 3)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 3; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed3) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2]);
}

static cl_object fixed_dispatch4 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[4];
  if (ecl_unlikely(narg != 4)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 4; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed4) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3]);
}

static cl_object fixed_dispatch5 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[5];
  if (ecl_unlikely(narg != 5)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 5; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed5) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4]);
}

static cl_object fixed_dispatch6 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[6];
  if (ecl_unlikely(narg != 6)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 6; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed6) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5]);
}

static cl_object fixed_dispatch7 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[7];
  if (ecl_unlikely(narg != 7)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 7; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed7) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6]);
}

static cl_object fixed_dispatch8 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[8];
  if (ecl_unlikely(narg != 8)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 8; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed8) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7]);
}

static cl_object fixed_dispatch9 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[9];
  if (ecl_unlikely(narg != 9)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 9; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed9) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8]);
}

static cl_object fixed_dispatch10 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[10];
  if (ecl_unlikely(narg != 10)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 10; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed10) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9]);
}

static cl_object fixed_dispatch11 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[11];
  if (ecl_unlikely(narg != 11)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 11; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed11) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10]);
}

static cl_object fixed_dispatch12 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[12];
  if (ecl_unlikely(narg != 12)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 12; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed12) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11]);
}

static cl_object fixed_dispatch13 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[13];
  if (ecl_unlikely(narg != 13)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 13; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed13) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12]);
}

static cl_object fixed_dispatch14 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[14];
  if (ecl_unlikely(narg != 14)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 14; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed14) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13]);
}

static cl_object fixed_dispatch15 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[15];
  if (ecl_unlikely(narg != 15)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 15; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed15) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14]);
}

static cl_object fixed_dispatch16 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[16];
  if (ecl_unlikely(narg != 16)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 16; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed16) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15]);
}

static cl_object fixed_dispatch17 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[17];
  if (ecl_unlikely(narg != 17)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 17; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed17) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16]);
}

static cl_object fixed_dispatch18 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[18];
  if (ecl_unlikely(narg != 18)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 18; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed18) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17]);
}

static cl_object fixed_dispatch19 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[19];
  if (ecl_unlikely(narg != 19)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 19; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed19) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18]);
}

static cl_object fixed_dispatch20 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[20];
  if (ecl_unlikely(narg != 20)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 20; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed20) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19]);
}

static cl_object fixed_dispatch21 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[21];
  if (ecl_unlikely(narg != 21)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 21; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed21) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20]);
}

static cl_object fixed_dispatch22 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[22];
  if (ecl_unlikely(narg != 22)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 22; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed22) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21]);
}

static cl_object fixed_dispatch23 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[23];
  if (ecl_unlikely(narg != 23)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 23; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed23) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22]);
}

static cl_object fixed_dispatch24 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[24];
  if (ecl_unlikely(narg != 24)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 24; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed24) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23]);
}

static cl_object fixed_dispatch25 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[25];
  if (ecl_unlikely(narg != 25)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 25; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed25) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24]);
}

static cl_object fixed_dispatch26 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[26];
  if (ecl_unlikely(narg != 26)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 26; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed26) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25]);
}

static cl_object fixed_dispatch27 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[27];
  if (ecl_unlikely(narg != 27)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 27; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed27) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26]);
}

static cl_object fixed_dispatch28 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[28];
  if (ecl_unlikely(narg != 28)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 28; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed28) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27]);
}

static cl_object fixed_dispatch29 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[29];
  if (ecl_unlikely(narg != 29)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 29; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed29) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28]);
}

static cl_object fixed_dispatch30 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[30];
  if (ecl_unlikely(narg != 30)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 30; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed30) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29]);
}

static cl_object fixed_dispatch31 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[31];
  if (ecl_unlikely(narg != 31)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 31; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed31) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30]);
}

static cl_object fixed_dispatch32 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[32];
  if (ecl_unlikely(narg != 32)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 32; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed32) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31]);
}

static cl_object fixed_dispatch33 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[33];
  if (ecl_unlikely(narg != 33)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 33; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed33) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32]);
}

static cl_object fixed_dispatch34 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[34];
  if (ecl_unlikely(narg != 34)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 34; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed34) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33]);
}

static cl_object fixed_dispatch35 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[35];
  if (ecl_unlikely(narg != 35)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 35; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed35) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34]);
}

static cl_object fixed_dispatch36 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[36];
  if (ecl_unlikely(narg != 36)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 36; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed36) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35]);
}

static cl_object fixed_dispatch37 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[37];
  if (ecl_unlikely(narg != 37)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 37; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed37) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36]);
}

static cl_object fixed_dispatch38 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[38];
  if (ecl_unlikely(narg != 38)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 38; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed38) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37]);
}

static cl_object fixed_dispatch39 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[39];
  if (ecl_unlikely(narg != 39)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 39; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed39) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38]);
}

static cl_object fixed_dispatch40 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[40];
  if (ecl_unlikely(narg != 40)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 40; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed40) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39]);
}

static cl_object fixed_dispatch41 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[41];
  if (ecl_unlikely(narg != 41)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 41; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed41) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40]);
}

static cl_object fixed_dispatch42 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[42];
  if (ecl_unlikely(narg != 42)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 42; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed42) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41]);
}

static cl_object fixed_dispatch43 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[43];
  if (ecl_unlikely(narg != 43)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 43; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed43) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42]);
}

static cl_object fixed_dispatch44 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[44];
  if (ecl_unlikely(narg != 44)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 44; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed44) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43]);
}

static cl_object fixed_dispatch45 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[45];
  if (ecl_unlikely(narg != 45)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 45; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed45) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44]);
}

static cl_object fixed_dispatch46 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[46];
  if (ecl_unlikely(narg != 46)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 46; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed46) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45]);
}

static cl_object fixed_dispatch47 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[47];
  if (ecl_unlikely(narg != 47)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 47; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed47) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46]);
}

static cl_object fixed_dispatch48 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[48];
  if (ecl_unlikely(narg != 48)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 48; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed48) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47]);
}

static cl_object fixed_dispatch49 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[49];
  if (ecl_unlikely(narg != 49)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 49; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed49) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48]);
}

static cl_object fixed_dispatch50 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[50];
  if (ecl_unlikely(narg != 50)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 50; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed50) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49]);
}

static cl_object fixed_dispatch51 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[51];
  if (ecl_unlikely(narg != 51)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 51; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed51) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50]);
}

static cl_object fixed_dispatch52 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[52];
  if (ecl_unlikely(narg != 52)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 52; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed52) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51]);
}

static cl_object fixed_dispatch53 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[53];
  if (ecl_unlikely(narg != 53)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 53; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed53) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52]);
}

static cl_object fixed_dispatch54 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[54];
  if (ecl_unlikely(narg != 54)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 54; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed54) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53]);
}

static cl_object fixed_dispatch55 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[55];
  if (ecl_unlikely(narg != 55)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 55; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed55) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54]);
}

static cl_object fixed_dispatch56 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[56];
  if (ecl_unlikely(narg != 56)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 56; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed56) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55]);
}

static cl_object fixed_dispatch57 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[57];
  if (ecl_unlikely(narg != 57)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 57; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed57) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56]);
}

static cl_object fixed_dispatch58 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[58];
  if (ecl_unlikely(narg != 58)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 58; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed58) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57]);
}

static cl_object fixed_dispatch59 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[59];
  if (ecl_unlikely(narg != 59)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 59; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed59) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58]);
}

static cl_object fixed_dispatch60 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[60];
  if (ecl_unlikely(narg != 60)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 60; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed60) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58], x[59]);
}

static cl_object fixed_dispatch61 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[61];
  if (ecl_unlikely(narg != 61)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 61; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed61) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58], x[59], x[60]);
}

static cl_object fixed_dispatch62 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[62];
  if (ecl_unlikely(narg != 62)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 62; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed62) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58], x[59], x[60], x[61]);
}

static cl_object fixed_dispatch63 (cl_narg narg, ...) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_object fun = the_env->function;
  va_list args;
  cl_object x[63];
  if (ecl_unlikely(narg != 63)) FEwrong_num_arguments(fun);

  va_start(args, narg);
  for (int i = 0; i < 63; i++)
    x[i] = va_arg(args, cl_object);
  va_end(args);

  return ((cl_objectfn_fixed63) fun->cfunfixed.entry_fixed)(x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58], x[59], x[60], x[61], x[62]);
}


static cl_objectfn fixed_dispatch_table[ECL_C_ARGUMENTS_LIMIT+1] = {
(cl_objectfn)fixed_dispatch0,
(cl_objectfn)fixed_dispatch1,
(cl_objectfn)fixed_dispatch2,
(cl_objectfn)fixed_dispatch3,
(cl_objectfn)fixed_dispatch4,
(cl_objectfn)fixed_dispatch5,
(cl_objectfn)fixed_dispatch6,
(cl_objectfn)fixed_dispatch7,
(cl_objectfn)fixed_dispatch8,
(cl_objectfn)fixed_dispatch9,
(cl_objectfn)fixed_dispatch10,
(cl_objectfn)fixed_dispatch11,
(cl_objectfn)fixed_dispatch12,
(cl_objectfn)fixed_dispatch13,
(cl_objectfn)fixed_dispatch14,
(cl_objectfn)fixed_dispatch15,
(cl_objectfn)fixed_dispatch16,
(cl_objectfn)fixed_dispatch17,
(cl_objectfn)fixed_dispatch18,
(cl_objectfn)fixed_dispatch19,
(cl_objectfn)fixed_dispatch20,
(cl_objectfn)fixed_dispatch21,
(cl_objectfn)fixed_dispatch22,
(cl_objectfn)fixed_dispatch23,
(cl_objectfn)fixed_dispatch24,
(cl_objectfn)fixed_dispatch25,
(cl_objectfn)fixed_dispatch26,
(cl_objectfn)fixed_dispatch27,
(cl_objectfn)fixed_dispatch28,
(cl_objectfn)fixed_dispatch29,
(cl_objectfn)fixed_dispatch30,
(cl_objectfn)fixed_dispatch31,
(cl_objectfn)fixed_dispatch32,
(cl_objectfn)fixed_dispatch33,
(cl_objectfn)fixed_dispatch34,
(cl_objectfn)fixed_dispatch35,
(cl_objectfn)fixed_dispatch36,
(cl_objectfn)fixed_dispatch37,
(cl_objectfn)fixed_dispatch38,
(cl_objectfn)fixed_dispatch39,
(cl_objectfn)fixed_dispatch40,
(cl_objectfn)fixed_dispatch41,
(cl_objectfn)fixed_dispatch42,
(cl_objectfn)fixed_dispatch43,
(cl_objectfn)fixed_dispatch44,
(cl_objectfn)fixed_dispatch45,
(cl_objectfn)fixed_dispatch46,
(cl_objectfn)fixed_dispatch47,
(cl_objectfn)fixed_dispatch48,
(cl_objectfn)fixed_dispatch49,
(cl_objectfn)fixed_dispatch50,
(cl_objectfn)fixed_dispatch51,
(cl_objectfn)fixed_dispatch52,
(cl_objectfn)fixed_dispatch53,
(cl_objectfn)fixed_dispatch54,
(cl_objectfn)fixed_dispatch55,
(cl_objectfn)fixed_dispatch56,
(cl_objectfn)fixed_dispatch57,
(cl_objectfn)fixed_dispatch58,
(cl_objectfn)fixed_dispatch59,
(cl_objectfn)fixed_dispatch60,
(cl_objectfn)fixed_dispatch61,
(cl_objectfn)fixed_dispatch62,
(cl_objectfn)fixed_dispatch63};

#ifdef ECL_C_COMPATIBLE_VARIADIC_DISPATCH

typedef cl_object (*cl_objectfn1)(cl_narg narg, cl_object, ...);
typedef cl_object (*cl_objectfn2)(cl_narg narg, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn3)(cl_narg narg, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn4)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn5)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn6)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn7)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn8)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn9)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn10)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn11)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn12)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn13)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn14)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn15)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn16)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn17)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn18)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn19)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn20)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn21)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn22)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn23)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn24)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn25)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn26)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn27)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn28)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn29)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn30)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn31)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn32)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn33)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn34)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn35)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn36)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn37)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn38)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn39)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn40)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn41)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn42)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn43)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn44)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn45)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn46)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn47)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn48)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn49)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn50)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn51)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn52)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn53)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn54)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn55)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn56)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn57)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn58)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn59)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn60)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn61)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn62)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);
typedef cl_object (*cl_objectfn63)(cl_narg narg, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, cl_object, ...);

#define DEFINE_VARIADIC_DISPATCH(n) \
  static cl_object variadic_dispatch ## n (cl_narg narg, ...) { \
    const cl_env_ptr the_env = ecl_process_env(); \
    cl_object fun = the_env->function; \
    va_list args; \
    cl_object x[ECL_C_ARGUMENTS_LIMIT + 1]; \
    va_start(args, narg); \
    for (int i = 0; \
         i < (narg < ECL_C_ARGUMENTS_LIMIT + 1 ? narg : ECL_C_ARGUMENTS_LIMIT + 1); \
         i++)                                                           \
      x[i] = va_arg(args, cl_object); \
    va_end(args); \
    return ((cl_objectfn ## n) fun->cfun.entry_variadic)(narg, x[0], x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12], x[13], x[14], x[15], x[16], x[17], x[18], x[19], x[20], x[21], x[22], x[23], x[24], x[25], x[26], x[27], x[28], x[29], x[30], x[31], x[32], x[33], x[34], x[35], x[36], x[37], x[38], x[39], x[40], x[41], x[42], x[43], x[44], x[45], x[46], x[47], x[48], x[49], x[50], x[51], x[52], x[53], x[54], x[55], x[56], x[57], x[58], x[59], x[60], x[61], x[62], x[63]); \
}

DEFINE_VARIADIC_DISPATCH(1)
DEFINE_VARIADIC_DISPATCH(2)
DEFINE_VARIADIC_DISPATCH(3)
DEFINE_VARIADIC_DISPATCH(4)
DEFINE_VARIADIC_DISPATCH(5)
DEFINE_VARIADIC_DISPATCH(6)
DEFINE_VARIADIC_DISPATCH(7)
DEFINE_VARIADIC_DISPATCH(8)
DEFINE_VARIADIC_DISPATCH(9)
DEFINE_VARIADIC_DISPATCH(10)
DEFINE_VARIADIC_DISPATCH(11)
DEFINE_VARIADIC_DISPATCH(12)
DEFINE_VARIADIC_DISPATCH(13)
DEFINE_VARIADIC_DISPATCH(14)
DEFINE_VARIADIC_DISPATCH(15)
DEFINE_VARIADIC_DISPATCH(16)
DEFINE_VARIADIC_DISPATCH(17)
DEFINE_VARIADIC_DISPATCH(18)
DEFINE_VARIADIC_DISPATCH(19)
DEFINE_VARIADIC_DISPATCH(20)
DEFINE_VARIADIC_DISPATCH(21)
DEFINE_VARIADIC_DISPATCH(22)
DEFINE_VARIADIC_DISPATCH(23)
DEFINE_VARIADIC_DISPATCH(24)
DEFINE_VARIADIC_DISPATCH(25)
DEFINE_VARIADIC_DISPATCH(26)
DEFINE_VARIADIC_DISPATCH(27)
DEFINE_VARIADIC_DISPATCH(28)
DEFINE_VARIADIC_DISPATCH(29)
DEFINE_VARIADIC_DISPATCH(30)
DEFINE_VARIADIC_DISPATCH(31)
DEFINE_VARIADIC_DISPATCH(32)
DEFINE_VARIADIC_DISPATCH(33)
DEFINE_VARIADIC_DISPATCH(34)
DEFINE_VARIADIC_DISPATCH(35)
DEFINE_VARIADIC_DISPATCH(36)
DEFINE_VARIADIC_DISPATCH(37)
DEFINE_VARIADIC_DISPATCH(38)
DEFINE_VARIADIC_DISPATCH(39)
DEFINE_VARIADIC_DISPATCH(40)
DEFINE_VARIADIC_DISPATCH(41)
DEFINE_VARIADIC_DISPATCH(42)
DEFINE_VARIADIC_DISPATCH(43)
DEFINE_VARIADIC_DISPATCH(44)
DEFINE_VARIADIC_DISPATCH(45)
DEFINE_VARIADIC_DISPATCH(46)
DEFINE_VARIADIC_DISPATCH(47)
DEFINE_VARIADIC_DISPATCH(48)
DEFINE_VARIADIC_DISPATCH(49)
DEFINE_VARIADIC_DISPATCH(50)
DEFINE_VARIADIC_DISPATCH(51)
DEFINE_VARIADIC_DISPATCH(52)
DEFINE_VARIADIC_DISPATCH(53)
DEFINE_VARIADIC_DISPATCH(54)
DEFINE_VARIADIC_DISPATCH(55)
DEFINE_VARIADIC_DISPATCH(56)
DEFINE_VARIADIC_DISPATCH(57)
DEFINE_VARIADIC_DISPATCH(58)
DEFINE_VARIADIC_DISPATCH(59)
DEFINE_VARIADIC_DISPATCH(60)
DEFINE_VARIADIC_DISPATCH(61)
DEFINE_VARIADIC_DISPATCH(62)
DEFINE_VARIADIC_DISPATCH(63)

static cl_objectfn variadic_dispatch_table[ECL_C_ARGUMENTS_LIMIT+1] = {
NULL,
(cl_objectfn)variadic_dispatch1,
(cl_objectfn)variadic_dispatch2,
(cl_objectfn)variadic_dispatch3,
(cl_objectfn)variadic_dispatch4,
(cl_objectfn)variadic_dispatch5,
(cl_objectfn)variadic_dispatch6,
(cl_objectfn)variadic_dispatch7,
(cl_objectfn)variadic_dispatch8,
(cl_objectfn)variadic_dispatch9,
(cl_objectfn)variadic_dispatch10,
(cl_objectfn)variadic_dispatch11,
(cl_objectfn)variadic_dispatch12,
(cl_objectfn)variadic_dispatch13,
(cl_objectfn)variadic_dispatch14,
(cl_objectfn)variadic_dispatch15,
(cl_objectfn)variadic_dispatch16,
(cl_objectfn)variadic_dispatch17,
(cl_objectfn)variadic_dispatch18,
(cl_objectfn)variadic_dispatch19,
(cl_objectfn)variadic_dispatch20,
(cl_objectfn)variadic_dispatch21,
(cl_objectfn)variadic_dispatch22,
(cl_objectfn)variadic_dispatch23,
(cl_objectfn)variadic_dispatch24,
(cl_objectfn)variadic_dispatch25,
(cl_objectfn)variadic_dispatch26,
(cl_objectfn)variadic_dispatch27,
(cl_objectfn)variadic_dispatch28,
(cl_objectfn)variadic_dispatch29,
(cl_objectfn)variadic_dispatch30,
(cl_objectfn)variadic_dispatch31,
(cl_objectfn)variadic_dispatch32,
(cl_objectfn)variadic_dispatch33,
(cl_objectfn)variadic_dispatch34,
(cl_objectfn)variadic_dispatch35,
(cl_objectfn)variadic_dispatch36,
(cl_objectfn)variadic_dispatch37,
(cl_objectfn)variadic_dispatch38,
(cl_objectfn)variadic_dispatch39,
(cl_objectfn)variadic_dispatch40,
(cl_objectfn)variadic_dispatch41,
(cl_objectfn)variadic_dispatch42,
(cl_objectfn)variadic_dispatch43,
(cl_objectfn)variadic_dispatch44,
(cl_objectfn)variadic_dispatch45,
(cl_objectfn)variadic_dispatch46,
(cl_objectfn)variadic_dispatch47,
(cl_objectfn)variadic_dispatch48,
(cl_objectfn)variadic_dispatch49,
(cl_objectfn)variadic_dispatch50,
(cl_objectfn)variadic_dispatch51,
(cl_objectfn)variadic_dispatch52,
(cl_objectfn)variadic_dispatch53,
(cl_objectfn)variadic_dispatch54,
(cl_objectfn)variadic_dispatch55,
(cl_objectfn)variadic_dispatch56,
(cl_objectfn)variadic_dispatch57,
(cl_objectfn)variadic_dispatch58,
(cl_objectfn)variadic_dispatch59,
(cl_objectfn)variadic_dispatch60,
(cl_objectfn)variadic_dispatch61,
(cl_objectfn)variadic_dispatch62,
(cl_objectfn)variadic_dispatch63};

#endif
