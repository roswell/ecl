/* This is free and unencumbered software released into the public domain. */

/**
 * Coverity Scan model for ECL.
 *
 * This is a modeling file for the Coverity Scan static analysis service.
 * Modeling helps to avoid false positives. Some rules for model files:
 *
 * - A model file cannot import any header files.
 * - Therefore only built-in standard language symbols are available;
 *   for example, `char`, `int`, and `void` are defined, `NULL` isn't.
 * - Modeling doesn't require genuine definitions for structs and typedefs.
 *   Typedefs that expand to rudimentary structs will suffice.
 * - Uninitialized local pointers are not errors, but rather signify that
 *   a variable could either be `NULL` or point at some data.
 *
 * Note that Coverity Scan doesn't automatically pick up changes to this
 * file. New versions of the model file must be uploaded by an admin at:
 * https://scan.coverity.com/projects/3235?tab=analysis_settings
 *
 * @author Arto Bendiken <arto@bendiken.net>
 * @see    https://scan.coverity.com/tune
 */

#define NULL ((void*)0)

typedef void* cl_object;
typedef int cl_narg;

void
FEwrong_type_nth_arg(cl_object function, cl_narg narg, cl_object value, cl_object type) {
  __coverity_panic__();
}
