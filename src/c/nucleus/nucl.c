
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

/* Test code */

cl_object nucl_flamethrower(int narg) {
  printf("nucl_flamethrower: throwing a flame!\n");
  ecl_signal(ECL_NIL, ecl_ct_dummy_tag, ECL_NIL);
  printf("nucl_flamethrower: resumed\n");
  return ECL_NIL;
}

static const struct ecl_cfunfixed _nucl_flamethrower_data = {
  (int8_t)t_cfunfixed, 0, 0, 0,
  /*name*/ECL_NIL, /*block*/ECL_NIL,
  /*entry*/(cl_objectfn)nucl_flamethrower,
  /*funfixed_entry*/(cl_objectfn_fixed)NULL,
  ECL_NIL, ECL_NIL };
static const cl_object _nucl_flamethrower = (cl_object)(&_nucl_flamethrower_data);

cl_object nucl_extinguisher(int narg, cl_object condition) {
  printf("nucl_extinguisher: coolio!\n");
  /* printf("UHOH, VITROL!\n"); */
  /* nucl_flamethrower(); */
  /* ecl_resume(); */
  return ECL_NIL;
}

static const struct ecl_cfunfixed _nucl_extinguisher_data = {
  (int8_t)t_cfunfixed, 0, 1, 0,
  /*name*/ECL_NIL, /*block*/ECL_NIL,
  /*entry*/(cl_objectfn)nucl_extinguisher,
  /*funfixed_entry*/(cl_objectfn_fixed)NULL,
  ECL_NIL, ECL_NIL };
static const cl_object _nucl_extinguisher = (cl_object)(&_nucl_extinguisher_data);

/* void ecl_debugger (cl_object condition) { */
/*   printf("Uh oh, congratulations, you've landed in the debugger!\n"); */
/*   printf("That means, that none of the other handlers decided to act.\n"); */
/*   printf("Can I print the condition? Well... %p\n", condition); */
/*   printf("Can you issue commands to _the debugger_? Um, no...\n"); */
/*   printf("Is it still a debugger in that case? Well... maybe?\n"); */
/*   ecl_frame_ptr fr = frs_sch(ecl_ct_resume_tag); */
/*   if (!fr) { */
/*     printf("Poor unfortunate soul, there are no restarts available.\n"); */
/*     printf("For no better way to handle this, I'll just crash!\n"); */
/*     _ecl_unexpected_return(); */
/*   } */
/*   printf("Luckily for you, brave soul, I've found the RESUME tag.\n"); */
/*   printf("This probably means that it will be automatically mended.\n"); */
/*   ecl_escape(fr); */
/* } */

int main() {
  cl_env_ptr the_env = ecl_core.first_env;
  ecl_boot();
  init_early_stacks(the_env);

  /* FIXME this should be implemented in init_process */
#ifdef ECL_THREADS
  {
    cl_index idx;
    cl_object *vector = (cl_object *)ecl_malloc(1024*sizeof(cl_object*));
    for(idx=0; idx<1024; idx++) {
      vector[idx] = ECL_NO_TL_BINDING;
    }
    the_env->bds_stack.tl_bindings_size = 1024;
    the_env->bds_stack.tl_bindings = vector;
  }
#endif

  printf("Hello ECL! %p\n", the_env);

  printf("[ on] handler --------------------------------\n");
  ecl_call_with_handler(_nucl_extinguisher, _nucl_flamethrower);
  printf("[off] handler --------------------------------\n");
  nucl_flamethrower(0);

  printf("Good bye ECL! %p\n", the_env);
  return 0;
}
