/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * slot_value.d - optimized implementation of SLOT-VALUE and (SETF SLOT-VALUE)
 *
 * Copyright (c) 2026 Daniel Kochmański
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

/* The method incorporated here is identical to how we optimize accessors:

   - specialized cache table
   - precomputed class stamp
   - inlined pre-cache

   We treat SLOT-NAME as a generic function that is specialized to the class of
   its instance. Moreover, when the slot name is known at compile time, we
   invoke the optimized function that doesn't do the reverse lookup NAME->GF.

   Another trick that we incorporate is detecting whether the instance has
   SLOT-VALUE-USING-CLASS second argument specialized to it. It is a separate
   bit in the header, so the check is cheap and we can correctly decide whether
   we should use optimized method, or fallback to the full MOP invocation.

   Effectively this should have the same performance as optimized accessors when
   SLOT-NAME is known at compile time.

   Then we could go one step further -- SLOT-VALUE can't be redefined (unlike
   generic functions), so we could inline per call-site. That can be handled as
   a compiler macro.

 */

#include <ecl/ecl.h>
#include <ecl/internal.h>

static inline void
ensure_up_to_date_instance(cl_object instance, cl_object clas)
{
  unlikely_if (instance->instance.stamp != clas->instance.class_stamp) {
    _ecl_funcall2(@'clos::update-instance', instance);
  }
}

static cl_object
slot_location(cl_object clas, cl_object instance, cl_object slot_name)
{
  cl_object table = ECL_CLASS_LOCATIONS(clas);
  unlikely_if (Null(table)) {
    /* Not a STD-CLASS. */
    return ECL_NIL;
  }
  return ecl_gethash_safe(slot_name, table, OBJNULL);
}

extern cl_object
cl_slot_value(cl_object instance, cl_object slot_name)
{
  cl_object clas = ECL_CLASS_OF(instance), value = OBJNULL;
  cl_object index = slot_location(clas, instance, slot_name);
  unlikely_if (index == OBJNULL || index == ECL_NIL) {
    return _ecl_funcall3(@'clos::sloth-value-get', instance, slot_name);
  }

  ensure_up_to_date_instance(instance, clas);
  value = (ECL_FIXNUMP(index)
           ? instance->instance.slots[ecl_fixnum(index)]
           : ECL_CONS_CAR(index));

  unlikely_if (value == ECL_UNBOUND) {
    value = _ecl_funcall4(@'slot-unbound', clas, instance, slot_name);
  }
  ecl_return1(ecl_process_env(), value);
}

extern cl_object
clos_slot_value_set(cl_object value, cl_object instance, cl_object slot_name)
{
  cl_object clas = ECL_CLASS_OF(instance);
  cl_object index = slot_location(clas, instance, slot_name);
  unlikely_if (index == OBJNULL || index == ECL_NIL) {
    return _ecl_funcall4(@'clos::sloth-value-set', value, instance, slot_name);
  }

  ensure_up_to_date_instance(instance, clas);
  if (ECL_FIXNUMP(index)) {
    instance->instance.slots[ecl_fixnum(index)] = value;
  } else {
    ECL_RPLACA(index, value);
  }

  ecl_return1(ecl_process_env(), value);
}
