
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>
#include <ecl/bytecodes.h>

ecl_def_constant(ecl_ct_resume_tag, ECL_NIL, "RESUME-TAG", 10);

cl_object pucl_extinguisher(int narg, cl_object condition) {
  printf("Uh oh, congratulations, you've landed in the debugger!\n");
  printf("That means, that none of the other handlers decided to act.\n");
  printf("- Can I print the condition? Well... %p\n", condition);
  printf("- Can you issue commands to _the debugger_? Um, no...\n");
  printf("- Is it still a debugger in that case? Well... maybe?\n\n");
  ecl_frame_ptr fr = frs_sch(ecl_ct_resume_tag);
  if (!fr) {
    printf("Poor unfortunate soul, there are no restarts available.\n");
    printf("For no better way to handle this, I'll just decline!\n");
    return ECL_NIL;
  }
  printf("Luckily for you, brave soul, I've found the RESUME tag.\n");
  printf("This probably means that it will be automatically mended.\n");
  ecl_escape(ecl_ct_resume_tag);
}

cl_object pucl_flamethrower(int narg) {
  printf(">>> pucl_flamethrower: throwing a flame!\n");
  ecl_signal(ECL_NIL, ECL_T, ECL_NIL);
  printf("<<< pucl_flamethrower: signal declined\n");
  return ECL_NIL;
}

cl_object pucl_mirror(int narg, cl_object element) {
  const cl_env_ptr the_env = ecl_process_env();
  printf(">>> pucl_mirror: %li!\n", ecl_fixnum(element));
  ecl_return1(the_env, element);
}

ecl_def_function(_pucl_flamethrower, pucl_flamethrower, static, const);
ecl_def_function(_pucl_extinguisher, pucl_extinguisher, static, const);
ecl_def_function(_pucl_mirror, pucl_mirror, static, const);

/* : smoke_bytecodes NOP POP CALL 1 POP CALL 0 EXIT ; */
/* _pucl_flamethrower 42 _pucl_mirror smoke_bytecodes */

static cl_opcode program [] = {
  OP_NOP,
  OP_POP,
  OP_CALL,
  1,
  OP_POP,                       /* reg0 = _pucl_flamethrower */
  OP_CALL,
  0,
  OP_EXIT
};

void smoke_bytecodes (void)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_bytecodes aux_codes[1];
  struct ecl_stack_frame aux_frame[1];
  cl_object f = ecl_cast_ptr(cl_object,aux_frame);
  cl_object o = ecl_cast_ptr(cl_object,aux_codes);
  ecl_stack_frame_open(the_env, f, 3);
  ecl_stack_frame_push(f, _pucl_flamethrower);
  ecl_stack_frame_push(f, ecl_make_fixnum(42));
  ecl_stack_frame_push(f, _pucl_mirror);
  o->bytecodes.t = t_bytecodes;
  o->bytecodes.name = ECL_NIL;
  o->bytecodes.definition = ECL_NIL;
  o->bytecodes.code_size = 2;
  o->bytecodes.code = ecl_cast_ptr(char*,program);
  o->bytecodes.data = ECL_NIL;
  o->bytecodes.flex = ECL_NIL;
  o->bytecodes.nlcl = ecl_make_fixnum(0);
  ecl_interpret(f, ECL_NIL, o);
  ecl_stack_frame_close(f);
}

void pucl_test (void)
{
  cl_env_ptr the_env = ecl_core.first_env;
  printf("\n[:handler t :restart t] -----------------------\n");
  ECL_CATCH_BEGIN(the_env, ecl_ct_resume_tag); {
    ecl_call_with_handler(_pucl_extinguisher, _pucl_flamethrower);
  } ECL_CATCH_END;
  printf("-----------------------------------------------\n\n");

  printf("\n[:handler t :restart nil] ---------------------\n");
  ecl_call_with_handler(_pucl_extinguisher, _pucl_flamethrower);
  printf("-----------------------------------------------\n\n");

  printf("\n[:handler nil] --------------------------------\n");
  pucl_flamethrower(0);
  printf("-----------------------------------------------\n\n");

  cl_object handlers = ecl_cons_stack(_pucl_extinguisher, ECL_NIL);
  ECL_SETQ(the_env, ECL_SIGNAL_HANDLERS, handlers);

  printf("\n[:bytecodes t] --------------------------------\n");
  smoke_bytecodes();
  printf("-----------------------------------------------\n\n");
}

/* -- Entry point ----------------------------------------------------------- */
int main() {
  cl_env_ptr the_env = ecl_core.first_env;
  ecl_set_option(ECL_OPT_BIND_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_BIND_STACK_SAFETY_AREA, 8);
  ecl_set_option(ECL_OPT_FRAME_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_FRAME_STACK_SAFETY_AREA, 8);
  ecl_set_option(ECL_OPT_LISP_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_LISP_STACK_SAFETY_AREA, 8);

  ecl_boot();
  ecl_add_module(ecl_module_process);
  ecl_add_module(ecl_module_stacks);

  printf("Hello PUCL! %p\n", the_env);
  pucl_test();
  printf("Good bye PUCL! %p\n", the_env);

  ecl_halt();
  return 0;
}
