
/* -- imports ------------------------------------------------------- */

#include <string.h>
#include <ecl/ecl.h>

/* -- Stack implementation ------------------------------------------ */

void
_ecl_stack_set_size(cl_object object, cl_index new_size, cl_index slip)
{
  cl_object *old_org = object->stack.org;
  cl_object *new_org;
  cl_index top = object->stack.top - object->stack.org;
  cl_index old_size = object->stack.size;
  cl_index _size = new_size + slip;
  if (ecl_unlikely(top > new_size)) {
    top = old_size = new_size;  /* chop the excessive elements */
  }
  /* It is the caller responsibility to disable interrupts if necessary. */
  new_org = (cl_object *)ecl_alloc_atomic(_size * sizeof(cl_object));
  memcpy(new_org, old_org, old_size * sizeof(cl_object));
  object->stack.slip = slip;
  object->stack.size = new_size;
  object->stack.org = new_org;
  object->stack.top = new_org + top;
  object->stack.end = new_org + new_size;
  ecl_dealloc(old_org);
}

void
_ecl_stack_grow(cl_object object)
{
  cl_index size = object->stack.size;
  cl_index slip = object->stack.slip;
  _ecl_stack_set_size(object, size + (size / 2), slip);
}

void
_ecl_stack_push(cl_object object, cl_object value)
{
  if (ecl_unlikely(object->stack.top >= object->stack.end)) {
    ecl_internal_error("_ecl_stack_push: stack overflow");
  }
  *object->stack.top = value;
  object->stack.top++;
}

void
_ecl_stack_drop(cl_object object, cl_index n)
{
  if (ecl_unlikely(object->stack.top - n <= object->stack.org)) {
    ecl_internal_error("_ecl_stack_pop: stack underflow");
  }
  object->stack.top -= n;
}

cl_object
_ecl_stack_pop(cl_object object)
{
  if (ecl_unlikely(object->stack.top <= object->stack.org)) {
    ecl_internal_error("_ecl_stack_pop: stack underflow");
  }
  return *(--object->stack.top);
}

cl_object
_ecl_stack_top(cl_object object)
{
  if (ecl_unlikely(object->stack.top <= object->stack.org)) {
    ecl_internal_error("_ecl_stack_top: stack underflow");
  }
  return *(object->stack.top-1);
}

cl_object
ecl_make_stack(cl_index size, cl_index slip) {
  cl_object object = ecl_alloc_object(t_stack);
  object->stack.org = object->stack.top = object->stack.end = NULL;
  object->stack.size = 0;
  _ecl_stack_set_size(object, size, slip);
  return object;
}
