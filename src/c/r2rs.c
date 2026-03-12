/* Implementation of R2RS, An UnCommonLisp.  */

/* -- imports --------------------------------------------------------------- */
#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>

cl_object ecl_make_nucl_stream(FILE *f);

/* -- globals --------------------------------------------------------------- */
static cl_object ostr = ECL_NIL; /* input stream */
static cl_object istr = ECL_NIL; /* output stream */
static cl_object rtab = ECL_NIL; /* reader table */


/* -- stream ---------------------------------------------------------------- */
void
init_r2rs_streams(void) {
  ostr = ecl_make_nucl_stream(stdout);
  istr = ecl_make_nucl_stream(stdin);
}


/* -- reader ---------------------------------------------------------------- */

static cl_object
alloc_readtable(void) {
  cl_object rtable = ecl_alloc_object(t_readtable);
  struct ecl_readtable_entry *rtab = (struct ecl_readtable_entry *)
    ecl_alloc(RTABSIZE * sizeof(struct ecl_readtable_entry));
  rtable->readtable.locked = 0;
  rtable->readtable.parse_token = NULL;
  rtable->readtable.read_case = ecl_case_upcase; /* enum ecl_readtable_case */
  rtable->readtable.table = rtab;
  for (int i = 0;  i < RTABSIZE;  i++) {
    rtab[i].syntax_type = cat_constituent; /* enum ecl_chattrib */
    rtab[i].macro = ECL_NIL;
    rtab[i].table = ECL_NIL;
  }
#ifdef ECL_UNICODE
  rtable->readtable.hash = ECL_NIL;
#endif
  return rtable;
}

static cl_object
r2rs_parse_token(cl_object token, cl_object in, int flags) {
  return ostr;
}

static cl_object
_niy(int narg, cl_object in, cl_object c, cl_object d)
{
  ecl_internal_error("not implemented yet");
}

static cl_object
_nif(int narg, cl_object in, cl_object c, cl_object d)
{
  ecl_internal_error("deliberely unspecified");
}

ecl_def_function(niy, _niy, static, const);
ecl_def_function(nif, _nif, static, const);

void
init_r2rs_reader(void) {
  cl_object niy = niy;
  cl_object nif = nif;
  rtab = alloc_readtable();
  rtab->readtable.parse_token = r2rs_parse_token;
  /* blanks */
  ecl_readtable_set(rtab, '\t', cat_whitespace, ECL_NIL, ECL_NIL);
  ecl_readtable_set(rtab, '\n', cat_whitespace, ECL_NIL, ECL_NIL);
  ecl_readtable_set(rtab, '\f', cat_whitespace, ECL_NIL, ECL_NIL);
  ecl_readtable_set(rtab, '\r', cat_whitespace, ECL_NIL, ECL_NIL);
  ecl_readtable_set(rtab, ' ',  cat_whitespace, ECL_NIL, ECL_NIL);
  /* Special characters */
  ecl_readtable_set(rtab, ')',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, '(',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, ']',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, '[',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, '}',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, '{',  cat_terminating, niy, ECL_NIL);
  ecl_readtable_set(rtab, '"',  cat_terminating, niy, ECL_NIL);
  /* Deliberately unspecified (we make them special) */
  ecl_readtable_set(rtab, '#',  cat_terminating, nif, ECL_NIL);
  ecl_readtable_set(rtab, '`',  cat_terminating, nif, ECL_NIL);
  ecl_readtable_set(rtab, '\'', cat_terminating, nif, ECL_NIL);
  ecl_readtable_set(rtab, '@',  cat_terminating, nif, ECL_NIL);
  ecl_readtable_set(rtab, '\\', cat_terminating, nif, ECL_NIL);
  ecl_readtable_set(rtab, '|',  cat_terminating, nif, ECL_NIL);
}


/* -- Entry point ----------------------------------------------------------- */

cl_object
r2rs_write_cstr(const char *s)
{
  cl_object strm = ostr;
  while(*s != '\0')
    ecl_write_char(*s++, strm);
}

cl_object
r2rs_write_object(cl_object self)
{
  if(self == OBJNULL) {
    r2rs_write_cstr("%OBJNULL");
    return self;
  }
  cl_type t = ecl_t_of(self);
  cl_object reg = ECL_NIL;
  int aux = 0;

  /* FIXME ecl_type_info[NIL] -> "CONS" */
  const char *name = ecl_type_info[t].name;
  r2rs_write_cstr("#<");
  r2rs_write_cstr(name);
  r2rs_write_cstr(">");
  return ECL_NIL;
}

void r2rs_repl(void) {
  cl_object result;
  while(1) {
    printf("> ");
    result = ecl_read_object_with_delimiter(rtab, istr, '\n', 0);
    r2rs_write_object(result);
  }
}

int main() {
  cl_env_ptr the_env = ecl_core.first_env;
  ecl_set_option(ECL_OPT_BIND_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_BIND_STACK_SAFETY_AREA, 8);
  ecl_set_option(ECL_OPT_FRAME_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_FRAME_STACK_SAFETY_AREA, 8);
  ecl_set_option(ECL_OPT_LISP_STACK_SIZE, 32);
  ecl_set_option(ECL_OPT_LISP_STACK_SAFETY_AREA, 8);

  the_env->string_pool = ECL_NIL;
  the_env->token_pool = ECL_NIL;

  ecl_boot();
  ecl_add_module(ecl_module_process);
  ecl_add_module(ecl_module_stacks);

  init_r2rs_streams();
  init_r2rs_reader();

  printf("Hello ECL! %p\n", the_env);
  r2rs_repl();
  printf("Good bye ECL! %p\n", the_env);

  ecl_halt();
  return 0;
}
