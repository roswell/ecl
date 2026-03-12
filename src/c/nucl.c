
/* -- imports ------------------------------------------------------- */

#include <ecl/ecl.h>
#include <ecl/ecl-inl.h>
#include <ecl/internal.h>
#include <ecl/external.h>
#include <ecl/bytecodes.h>
#include <string.h>             /* for nucl_strcmp */

/* -- Allocators ------------------------------------------------------------ */

cl_object
nucl_alloc_base_string(cl_index s)
{
  cl_object x = ecl_alloc_compact_object(t_base_string, s+1);
  x->base_string.self = ECL_COMPACT_OBJECT_EXTRA(x);
  x->base_string.self[s] = '\0';
  x->base_string.elttype = ecl_aet_bc;
  x->base_string.flags = 0; /* no fill pointer, not adjustable */
  x->base_string.displaced = ECL_NIL;
  x->base_string.dim = x->base_string.fillp = s;
  return x;
}

cl_object
nucl_alloc_symbol(cl_object name, cl_object value)
{
  cl_object x = ecl_alloc_object(t_symbol);
  x->symbol.name = name;
  x->symbol.cname = ECL_NIL;
#ifdef ECL_THREADS
  x->symbol.binding = ECL_MISSING_SPECIAL_BINDING;
#endif
  ECL_SET(x,value);
  ECL_FMAKUNBOUND(x);
  x->symbol.undef_entry = NULL; /* ecl_undefined_function_entry */
  x->symbol.macfun = ECL_NIL;
  x->symbol.sfdef = ECL_NIL;
  x->symbol.plist = ECL_NIL;
  x->symbol.hpack = ECL_NIL;
  x->symbol.stype = ecl_stp_ordinary;
#ifndef ECL_NUCL
  /* Rethink finalization(!) */
  ecl_set_finalizer_unprotected(x, ECL_T);
#endif
  return x;
}

cl_object
nucl_alloc_readtable(void) {
  cl_object rtable = ecl_alloc_object(t_readtable);
  struct ecl_readtable_entry *rtab = (struct ecl_readtable_entry *)
    ecl_alloc(RTABSIZE * sizeof(struct ecl_readtable_entry));
  rtable->readtable.locked = 0;
  rtable->readtable.read_case = ecl_case_preserve; /* enum ecl_readtable_case */
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

cl_object
nucl_alloc_bytecodes(void) {
  cl_object self = ecl_alloc_object(t_bytecodes);
  self->bytecodes.t = t_bytecodes;
  self->bytecodes.name = ECL_NIL;
  self->bytecodes.definition = ECL_NIL;
  self->bytecodes.code_size = 0;
  self->bytecodes.code = NULL;
  self->bytecodes.data = ECL_NIL;
  self->bytecodes.flex = ECL_NIL;
  self->bytecodes.nlcl = ecl_make_fixnum(0);
  /* self->bytecodes.entry = _nucl_word_dispatch; */
  return self;
}

/* -- Special variables ----------------------------------------------------- */
#define DEFINE_SPECIAL(var,name,value)                                  \
  static cl_object var =                                                \
    ecl_cast_ptr(cl_object, &ecl_constexpr_symbol(ecl_stp_special, name, value))


/* -- Stack manipulation ---------------------------------------------------  */

DEFINE_SPECIAL(nucl_fp, "*FP*", ECL_NIL); /* stack frame */

static struct ecl_stack_frame nucl_frames[64];
static cl_index sfi = 0;
cl_object
open_nucl_frame(void) {
  const cl_env_ptr the_env = ecl_process_env();
  if(sfi==64) ecl_internal_error("Stack frame overflow :3");
  cl_object f = ecl_cast_ptr(cl_object, &nucl_frames[sfi++]);
  ecl_stack_frame_open(the_env, f, 0);
  ecl_bds_bind(the_env, nucl_fp, f);
  return f;
}

void
close_nucl_frame(void) {
  const cl_env_ptr the_env = ecl_process_env();
  if(sfi==0) ecl_internal_error("Stack frame underflow :3");
  cl_object f = ECL_SYM_VAL(the_env, nucl_fp);
  ecl_bds_unwind1(the_env);
  ecl_stack_frame_close(f);
  sfi--;
}

static cl_object
nucl_stack_frame(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  return ECL_SYM_VAL(the_env, nucl_fp);
}

static cl_index
nucl_frame_size(cl_object fp)
{
  return fp->frame.sp - fp->frame.base;
}

static cl_index
nucl_stack_size(void)
{
  return nucl_frame_size(nucl_stack_frame());
}

static cl_object
nucl_stack_push(cl_object object)
{
  cl_object frame = nucl_stack_frame();
  ecl_stack_frame_push(frame, object);
  return object;
}

static cl_object
nucl_stack_pop(void)
{
  cl_object frame = nucl_stack_frame();
  return ecl_stack_frame_pop(frame);
}

static cl_object
nucl_stack_from_values(void)
{
  cl_object frame = nucl_stack_frame();
  ecl_stack_frame_push_values(frame);
}

static cl_object
nucl_stack_into_values(void)
{
  cl_object frame = nucl_stack_frame();
  ecl_stack_frame_pop_values(frame);
}

static void
nucl_stack_clear(void)
{
  cl_object frame = nucl_stack_frame();
  cl_index size = nucl_frame_size(frame);
  while(size--) ecl_stack_frame_pop(frame);
}

/* ( lisp* -- list ) */
cl_object nucl_stack_to_list(void)
{
  cl_object frame = nucl_stack_frame();
  cl_object self = ECL_NIL;
  loop_across_frame_filo(elt, frame) {
    self = ecl_cons(elt, self);
  } end_loop_across_frame();
  nucl_stack_clear();
  return nucl_stack_push(self);
}

/* ( char* -- string ) */
cl_object nucl_stack_to_string(void)
{
  cl_object frame = nucl_stack_frame();
  cl_index size = nucl_frame_size(frame), idx=0;
  cl_object self = nucl_alloc_base_string(size);
  loop_across_frame_fifo(elt, frame) {
    self->base_string.self[idx++] = ECL_CHAR_CODE(elt);
  } end_loop_across_frame();
  nucl_stack_clear();
  return nucl_stack_push(self);
}

/* ( char* -- fixnum ) */
cl_object nucl_stack_to_fixnum(void)
{
  cl_object frame = nucl_stack_frame();
  cl_object self = ECL_NIL;
  intmax_t acc = 0;
  int dig;
  loop_across_frame_fifo(elt, frame) {
    acc *= 10;
    switch(ECL_CHAR_CODE(elt)) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      acc += ECL_CHAR_CODE(elt) - '0';
      break;
    default:
      ecl_internal_error("Character is not a digit!");
    }
    if (acc>MOST_POSITIVE_FIXNUM)
      ecl_internal_error("Integer is too big!");
  } end_loop_across_frame();
  nucl_stack_clear();
  self = ecl_make_fixnum((cl_fixnum)acc);
  return nucl_stack_push(self);
}

/* ( char* -- fixnum ) */
cl_object nucl_stack_to_hexnum(void)
{
  cl_object frame = nucl_stack_frame();
  cl_object self = ECL_NIL;
  intmax_t acc = 0;
  int dig;
  loop_across_frame_fifo(elt, frame) {
    acc *= 16;
    switch(ECL_CHAR_CODE(elt)) {
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      acc += ECL_CHAR_CODE(elt) - '0';
      break;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      acc += 10 + (ECL_CHAR_CODE(elt) - 'a');
      break;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      acc += 10 + (ECL_CHAR_CODE(elt) - 'A');
      break;
    default:
      ecl_internal_error("Character is not a digit!");
    }
    if (acc>MOST_POSITIVE_FIXNUM)
      ecl_internal_error("Integer is too big!");
  } end_loop_across_frame();
  nucl_stack_clear();
  self = ecl_make_fixnum((cl_fixnum)acc);
  return nucl_stack_push(self);
}



/* -- Lali-ho I/O starts here ----------------------------------------------- */

cl_object ecl_make_nucl_stream(FILE *f);
DEFINE_SPECIAL(nucl_ostrm, "*ISTRM*", ECL_NIL); /* standard input */
DEFINE_SPECIAL(nucl_istrm, "*OSTRM*", ECL_NIL); /* standard output */

void
init_nucl_io(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object ostrm = ecl_make_nucl_stream(stdout);
  cl_object istrm = ecl_make_nucl_stream(stdin);
  ECL_SETQ(the_env, nucl_ostrm, ostrm);
  ECL_SETQ(the_env, nucl_istrm, istrm);
}

#define ECL_EOF ECL_DUMMY_TAG

cl_object
nucl_stdout(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  return ECL_SYM_VAL(the_env, nucl_ostrm);
}


cl_object
nucl_stdin(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  return ECL_SYM_VAL(the_env, nucl_istrm);
}

cl_object
nucl_write_cstr(const char *s)
{
  cl_object strm = nucl_stdout();
  while(*s != '\0')
    ecl_write_char(*s++, strm);
}

cl_object
nucl_write_char(const char ch)
{
  cl_object strm = nucl_stdout();
  ecl_write_char(ch, strm);
}

cl_object
nucl_read_char(cl_object eof_value)
{
  cl_object strm = nucl_stdin();
  ecl_character ch = ecl_read_char(strm);
  return (ch==EOF) ? eof_value : ECL_CODE_CHAR(ch);
}

cl_object
nucl_unread_char(cl_object ch)
{
  cl_object strm = nucl_stdin();
  if (ecl_unlikely(!ECL_CHARACTERP(ch))) {
    return ECL_NIL;
  }
  ecl_unread_char(ECL_CHAR_CODE(ch), strm);
  return ch;
}

cl_object
nucl_peek_char(cl_object eof_value)
{
  cl_object strm = nucl_stdin();
  ecl_character ch = ecl_peek_char(strm);
  return (ch==EOF) ? eof_value : ECL_CODE_CHAR(ch);
}

/* -- Ad-hoc writer --------------------------------------------------------- */
void
nucl_write_string(cl_object s)
{
  cl_object strm = nucl_stdout();
  cl_index aux = 0;
  for(; aux < s->string.fillp; aux++)
    (s->d.t == t_string)
      ? ecl_write_char(s->string.self[aux], strm)
      : ecl_write_char(s->base_string.self[aux], strm);
}

void
nucl_write_fixnum(cl_object s)
{
  cl_object strm = nucl_stdout();
  cl_object frame = open_nucl_frame();
  cl_object c=ECL_NIL;
  cl_fixnum value = ecl_fixnum(s), dig;
  if(value<0) value = -value;
  do {
    dig = value%10;
    nucl_stack_push(ECL_CODE_CHAR(dig+'0'));
    value /= 10;
  } while(value!=0);
  if(ecl_fixnum(s) < 0)
    nucl_stack_push(ECL_CODE_CHAR('-'));
  loop_across_frame_filo(elt, frame) {
    nucl_write_char(ECL_CHAR_CODE(elt));
  } end_loop_across_frame();
  close_nucl_frame();
}

cl_object
nucl_write_object(cl_object self)
{
  cl_object strm = nucl_stdout();
  if(self == OBJNULL) {
    nucl_write_cstr("%OBJNULL");
    return self;
  }
  cl_type t = ecl_t_of(self);
  cl_object reg = ECL_NIL;
  int aux = 0;
  switch (t) {
  case t_character:
    nucl_write_cstr("#\\");
    switch(ECL_CHAR_CODE(self)) {
    case '\n': nucl_write_cstr("Newline"); break;
    case ' ':  nucl_write_cstr("Space"); break;
    default:   nucl_write_char(ECL_CHAR_CODE(self)); }
    break;
  case t_fixnum:
    nucl_write_fixnum(self);
    break;
  case t_base_string:
  case t_string:
    nucl_write_char('"');
    nucl_write_string(self);
    nucl_write_char('"');
    break;
  case t_symbol:                /* ignores packages, introduce t_token? */
    reg = self->symbol.name;
    nucl_write_string(reg);
    break;
  case t_vector:
    nucl_write_char('[');
    loop_across_stack_fifo(elt, self) {
      nucl_write_object(elt);
      if(++aux < self->vector.fillp)
        nucl_write_char(' ');
    } end_loop_across_stack();
    nucl_write_char(']');
    break;
  case t_list:
    nucl_write_char('(');
    loop_for_on_unsafe(self) {
      reg = ECL_CONS_CAR(self);
      nucl_write_object(reg);
      reg = ECL_CONS_CDR(self);
      if (ECL_CONSP(reg)) {
        nucl_write_char(' ');
      } else if (!Null(reg)) {
        nucl_write_cstr(" . ");
        nucl_write_object(reg);
      }
    } end_loop_for_on_unsafe(self);
    nucl_write_char(')');
    break;
  default:
    {
      const char *name = ecl_type_info[t].name;
      nucl_write_cstr("#<");
      nucl_write_cstr(name);
      nucl_write_cstr(">");
      return ECL_NIL;
    }
  }
}

/* -- Ad-hoc reader --------------------------------------------------------- */
void
nucl_read_until(cl_fixnum delim)
{
  cl_object frame = nucl_stack_frame();
  cl_object ch = ECL_NIL;
  while (!Null(ch = nucl_read_char(ECL_NIL))) {
    nucl_stack_push(ch);
    if(ECL_CHAR_CODE(ch) == delim) break;
  }
}

cl_object
nucl_read_line()
{
  nucl_read_until('\n');
  return nucl_stack_to_string();
}

static cl_object rtable = ECL_NIL;
static cl_object nucl_accept(cl_object delim);

void
nucl_readtable_set(cl_object self, cl_fixnum c, enum ecl_chattrib cat,
                   cl_object macro)
{
  if (c >= RTABSIZE) {
    ecl_internal_error("Character is too big!");
  }
  self->readtable.table[c].macro = macro;
  self->readtable.table[c].table = ECL_NIL;
  self->readtable.table[c].syntax_type = cat;
}

struct ecl_readtable_entry *
nucl_readtable_get(cl_object self, cl_fixnum ch)
{
  if (ch >= RTABSIZE) {
    ecl_internal_error("Character is too big!");
  }
  return self->readtable.table+ch;
}

static void
default_reader(int narg, cl_object ch)
{
  /* This reader reads either a token (symbol) or a number (fixnum). Common Lisp
     concerns itself with read-base, but we are going to ditch it in favor of a
     simpler interpretation -- digit first means a fixnum, otherwise a symbol.
     If the first digit is 0, then it must be a hexadecimal number, i.e 0xff. */
  struct ecl_readtable_entry *entry = nucl_readtable_get(rtable, ECL_CHAR_CODE(ch));
  nucl_stack_push(ch);
  while (!Null(ch = nucl_read_char(ECL_NIL))) {
    entry = nucl_readtable_get(rtable, ECL_CHAR_CODE(ch));
    switch (entry->syntax_type) {
    case cat_constituent:
      nucl_stack_push(ch);
      break;
    case cat_terminating:
      nucl_unread_char(ch);
      return;
    case cat_whitespace:
      nucl_unread_char(ch);
      return;
    default:
      ecl_internal_error("Expecting too much, aren't we?");
    }
  }
}

static cl_object
limited_reader(cl_object delim)
{
  cl_object object;
  do {
    object = nucl_accept(delim);
    if(object == ECL_EOF || ecl_eql(object, delim))
      return object;
    else
      nucl_stack_push(object);
  } while(1);
  return ECL_NIL;
}

static cl_object
lparen_reader(int narg, cl_object c)
{
  limited_reader(ECL_CODE_CHAR(')'));
  return nucl_stack_to_list();
}

static cl_object
rparen_reader(int narg, cl_object c)
{
  ecl_internal_error("rparen reader");
}

static cl_object
symbol_reader(int narg, cl_object c)
{
  cl_object string, symbol;
  default_reader(1, c);
  nucl_stack_to_string();
  string = nucl_stack_pop();
  symbol = nucl_alloc_symbol(string, OBJNULL);
  return nucl_stack_push(symbol);
}

static cl_object
string_reader(int narg, cl_object c)
{
  nucl_read_until(ECL_CHAR_CODE(c));
  nucl_stack_pop();             /* remove delimiter */
  return nucl_stack_to_string();
}

static cl_object
fixnum_reader(int narg, cl_object c)
{
  default_reader(1, c);
  return nucl_stack_to_fixnum();
}

static cl_object
hexnum_reader(int narg, cl_object c)
{
  cl_object ch = nucl_read_char(ECL_NIL);
  if (Null(ch)) {
    return ecl_make_fixnum(0);
  } else if (ECL_CHAR_CODE(ch) != 'x' && ECL_CHAR_CODE(ch) != 'X')  {
    nucl_unread_char(ch);
    return ecl_make_fixnum(0);
  } else {
    default_reader(1, c);
    return nucl_stack_to_hexnum();
  }
}

ecl_def_function(_lparen_reader, lparen_reader, static, const);
ecl_def_function(_rparen_reader, rparen_reader, static, const);
ecl_def_function(_symbol_reader, symbol_reader, static, const);
ecl_def_function(_string_reader, string_reader, static, const);
ecl_def_function(_fixnum_reader, fixnum_reader, static, const);
ecl_def_function(_hexnum_reader, hexnum_reader, static, const);

/* Super extra super sauce -- depending on the character next character we
   either parse a symbol, a fixnum or a hexnum, where the first character in the
   case of numbers specifies the sign. */
static cl_object
mixnum_reader(int narg, cl_object c)
{
  cl_object reg = nucl_read_char(ECL_NIL);
  struct ecl_readtable_entry *entry
    = nucl_readtable_get(rtable, ECL_CHAR_CODE(reg));
  if(entry->macro == _fixnum_reader || entry->macro == _hexnum_reader) {
    reg = _ecl_funcall2(entry->macro, reg);
    return ecl_eql(c, ECL_CODE_CHAR('-'))
      ? ecl_make_fixnum(-ecl_fixnum(reg))
      : reg;
  }
  return symbol_reader(1, c);
}
ecl_def_function(_mixnum_reader, mixnum_reader, static, const);

void
init_nucl_reader(void)
{
  rtable = nucl_alloc_readtable(); /* FIXME initializes global var */
  struct ecl_readtable_entry *rtab = rtable->readtable.table;

  for (int i = 0;  i < RTABSIZE;  i++) {
    rtab[i].syntax_type = cat_constituent;
    rtab[i].macro = _symbol_reader;
    rtab[i].table = ECL_NIL;
  }

  for (char *s="123456789"; *s!='\0'; s++)
    rtab[*s].macro = _fixnum_reader;

  rtab['0'].macro = _hexnum_reader;
  rtab['-'].macro = _mixnum_reader;
  rtab['+'].macro = _mixnum_reader;

  nucl_readtable_set(rtable, '\t', cat_whitespace, ECL_NIL);
  nucl_readtable_set(rtable, '\n', cat_whitespace, ECL_NIL);
  nucl_readtable_set(rtable, '\f', cat_whitespace, ECL_NIL);
  nucl_readtable_set(rtable, '\r', cat_whitespace, ECL_NIL);
  nucl_readtable_set(rtable, ' ', cat_whitespace,  ECL_NIL);

  nucl_readtable_set(rtable, '"', cat_terminating, _string_reader);
  nucl_readtable_set(rtable, '(', cat_terminating, _lparen_reader);
  nucl_readtable_set(rtable, ')', cat_terminating, _rparen_reader);
}

static cl_object
skip_whitespace(cl_object delim)
{
  struct ecl_readtable_entry *entry = NULL;
  cl_object ch = ECL_NIL;
  do {
    ch = nucl_read_char(ECL_EOF);
    if (ch == ECL_EOF || ecl_eql(ch, delim)) return ch;
    entry = nucl_readtable_get(rtable, ECL_CHAR_CODE(ch));
    if (entry->syntax_type != cat_whitespace) {
      return ch;
    }
  } while(1);
}

static cl_object
nucl_accept(cl_object delim)
{
  cl_object frame = open_nucl_frame();
  struct ecl_readtable_entry *entry = NULL;
  cl_object ch = ECL_NIL;
  cl_object result = ECL_NIL;
  ch = skip_whitespace(delim);
  if (ch == ECL_EOF || ecl_eql(delim, ch)) {
    close_nucl_frame();
    return ch;
  }
  entry = nucl_readtable_get(rtable, ECL_CHAR_CODE(ch));
  switch (entry->syntax_type) {
  case cat_constituent:
    /* Here's some nuance -- if the first constituent character has an
       associated dispatch function, then we use it instead of a default
       reader. In our case this always happens. */
    if(Null(entry->macro))
      default_reader(1, ch);
    else
      result = _ecl_funcall2(entry->macro, ch);
    break;
  case cat_terminating:
    result = _ecl_funcall2(entry->macro, ch);
    break;
  default:
    ecl_internal_error("Expecting too much, aren't we?");
  }
  close_nucl_frame();
  return result;
}


/* -- F42 starts here ------------------------------------------------------- */

DEFINE_SPECIAL(nucl_dt, "*DT*", ECL_NIL);  /* dictionary */
DEFINE_SPECIAL(nucl_cmpp, "*CMPP*", ECL_NIL); /* compilep */

cl_object
get_definition_p(void) {
  const cl_env_ptr the_env = ecl_process_env();
  return ECL_SYM_VAL(the_env, nucl_cmpp);
}

cl_object
set_definition_p(cl_object value) {
  const cl_env_ptr the_env = ecl_process_env();
  return ECL_SETQ(the_env, nucl_cmpp, value);
}

static void
init_nucl_dictionary(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object dict = ecl_make_stack(0);
  ECL_SETQ(the_env, nucl_dt, dict);
}

static void
free_nucl_dictionary(void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object dict = ECL_SYM_VAL(the_env, nucl_dt);
  ecl_free_stack(dict);
  ECL_SETQ(the_env, nucl_dt, ECL_NIL);
}


static bool
nucl_strcmp(cl_object x, cl_object y)
{
  /* only base strings */
  cl_index
    fp1 = x->base_string.fillp,
    fp2 = y->base_string.fillp;
  if(fp1==fp2)
    return memcmp(x->base_string.self, y->base_string.self, fp1);
  else
    return 1;
}

static bool
nucl_cstrcmp(cl_object x, const char *s, cl_index fp2)
{
  /* only base strings */
  cl_index fp1 = x->base_string.fillp;
  if(fp1==fp2)
    return memcmp(x->base_string.self, s, fp1);
  else
    return 1;
}

static cl_object
nucl_search_dictionary(cl_object name)
{
  /* We iterate from the last element to allow shadowing if we decide on it. */
  const cl_env_ptr the_env = ecl_process_env();
  cl_object dict = ECL_SYM_VAL(the_env, nucl_dt);
  loop_across_stack_fifo(symbol, dict) {
    cl_object s_name = symbol->symbol.name;
    if (nucl_strcmp(name, s_name) == 0)
      return symbol;
  } end_loop_across_stack();
  return ECL_UNBOUND;
}

static cl_object
nucl_maybe_dictionary(cl_object self)
{
  cl_object sym = nucl_search_dictionary(self->symbol.name);
  return sym==ECL_UNBOUND ? self : sym;
}

static cl_object
nucl_append_dictionary(cl_object symbol, cl_object value)
{
  cl_env_ptr the_env = ecl_core.first_env;
  cl_object dict = ECL_SYM_VAL(the_env, nucl_dt);
  ECL_SET(symbol, value);
  ecl_stack_push(dict, symbol);
  return symbol;
}

/* -- Default words --------------------------------------------------------- */

cl_object nucl_word_print_dictionary(int narg) {
  cl_env_ptr the_env = ecl_core.first_env;
  cl_object dict = ECL_SYM_VAL(the_env, nucl_dt);
  loop_across_stack_fifo(elt, dict) {
    nucl_write_object(elt);
    nucl_write_cstr(" ");
  } end_loop_across_stack();
  return ECL_NIL;
}

cl_object nucl_word_print_stack(int narg) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_index ssize = ECL_STACK_INDEX(the_env);
  /* the stack always has 0 at the beginning */
  cl_object size = ecl_make_fixnum(ssize);
  cl_object *ptr;
  nucl_write_cstr("[");
  nucl_write_object(size);
  nucl_write_cstr("] ");
  for(ptr = the_env->run_stack.org;
      ptr < the_env->run_stack.top;
      ptr++) {
    nucl_write_object(*ptr);
    nucl_write_cstr(" ");
  }
  return size;
}

cl_object nucl_word_print_values(int narg) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_index ssize = the_env->nvalues, idx;
  cl_object size = ecl_make_fixnum(ssize);
  nucl_write_cstr("[");
  nucl_write_object(size);
  nucl_write_cstr("] ");
  for(idx=0; idx<ssize; idx++) {
    nucl_write_object(the_env->values[idx]);
    nucl_write_cstr(" ");
  }
  return size;
}

cl_object nucl_word_pop_and_print(int narg) {
  const cl_env_ptr the_env = ecl_process_env();
  cl_index ssize = ECL_STACK_INDEX(the_env);
  /* INV the first element 0 is always present. */
  if(ssize <= 1) {
    nucl_write_cstr("error: stack underflow");
    return ECL_NIL;
  } else {
    cl_object elt = ECL_STACK_POP_UNSAFE(the_env);
    nucl_write_object(elt);
    nucl_write_cstr(" ");
    return elt;
  }
}

ecl_def_function(_nucl_word_print_dictionary, nucl_word_print_dictionary, static, const);
ecl_def_function(_nucl_word_print_stack, nucl_word_print_stack, static, const);
ecl_def_function(_nucl_word_print_values, nucl_word_print_values, static, const);

ecl_def_function(_nucl_word_pop_and_print, nucl_word_pop_and_print, static, const);

/* Our dictionary is based on symbols. Each symbol in a dictionary has a name
   and a value. We may need to rethink it at some point.  */

#define make_dict_entry(name,value) \
  ecl_cast_ptr(cl_object, &ecl_constexpr_symbol(ecl_stp_special, name, value))

#define make_code_entry(name,code) \
  make_dict_entry(name, ecl_make_fixnum(code))

static const cl_object _nucl_sym_def = make_dict_entry(":", ECL_UNBOUND);
static const cl_object _nucl_sym_fed = make_dict_entry(";", ECL_UNBOUND);

static cl_object nucl_dictionary_default_entries[] = {
  _nucl_sym_def,
  _nucl_sym_fed,
  /* opcodes */
  make_code_entry("NOP", OP_NOP),
  make_code_entry("QUOTE", OP_QUOTE),
  make_code_entry("CALLW", OP_CALLW),
  make_code_entry("ENDP", OP_ENDP),
  make_code_entry("CONS", OP_CONS),
  make_code_entry("CAR", OP_CAR),
  make_code_entry("CDR", OP_CDR),
  make_code_entry("LIST", OP_LIST),
  make_code_entry("LISTA", OP_LISTA),
  make_code_entry("CONS_CAR", OP_CONS_CAR),
  make_code_entry("CONS_CDR", OP_CONS_CDR),
  make_code_entry("INT", OP_INT),
  make_code_entry("PINT", OP_PINT),
  make_code_entry("VAR", OP_VAR),
  make_code_entry("VARC", OP_VARC),
  make_code_entry("VARS", OP_VARS),
  make_code_entry("PUSH", OP_PUSH),
  make_code_entry("PUSHV", OP_PUSHV),
  make_code_entry("PUSHVC", OP_PUSHVC),
  make_code_entry("PUSHVS", OP_PUSHVS),
  make_code_entry("PUSHQ", OP_PUSHQ),
  make_code_entry("CALLG1", OP_CALLG1),
  make_code_entry("CALLG2", OP_CALLG2),
  make_code_entry("CALL", OP_CALL),
  make_code_entry("CALLG", OP_CALLG),
  make_code_entry("FCALL", OP_FCALL),
  make_code_entry("MCALL", OP_MCALL),
  make_code_entry("POP", OP_POP),
  make_code_entry("POP1", OP_POP1),
  make_code_entry("POPREQ", OP_POPREQ),
  make_code_entry("POPOPT", OP_POPOPT),
  make_code_entry("NOMORE", OP_NOMORE),
  make_code_entry("POPREST", OP_POPREST),
  make_code_entry("PUSHKEYS", OP_PUSHKEYS),
  make_code_entry("EXIT", OP_EXIT),
  make_code_entry("FLET", OP_FLET),
  make_code_entry("LABELS", OP_LABELS),
  make_code_entry("LFUNCTION", OP_LFUNCTION),
  make_code_entry("CFUNCTION", OP_CFUNCTION),
  make_code_entry("FUNCTION", OP_FUNCTION),
  make_code_entry("CLOSE", OP_CLOSE),
  make_code_entry("GO", OP_GO),
  make_code_entry("GO_CFB", OP_GO_CFB),
  make_code_entry("RETURN", OP_RETURN),
  make_code_entry("RETURN_CFB", OP_RETURN_CFB),
  make_code_entry("THROW", OP_THROW),
  make_code_entry("JMP", OP_JMP),
  make_code_entry("JNIL", OP_JNIL),
  make_code_entry("JT", OP_JT),
  make_code_entry("JEQL", OP_JEQL),
  make_code_entry("JNEQL", OP_JNEQL),
  make_code_entry("UNBIND", OP_UNBIND),
  make_code_entry("UNBINDS", OP_UNBINDS),
  make_code_entry("BIND", OP_BIND),
  make_code_entry("PBIND", OP_PBIND),
  make_code_entry("VBIND", OP_VBIND),
  make_code_entry("BINDS", OP_BINDS),
  make_code_entry("PBINDS", OP_PBINDS),
  make_code_entry("VBINDS", OP_VBINDS),
  make_code_entry("SETQ", OP_SETQ),
  make_code_entry("SETQC", OP_SETQC),
  make_code_entry("SETQS", OP_SETQS),
  make_code_entry("PSETQ", OP_PSETQ),
  make_code_entry("PSETQC", OP_PSETQC),
  make_code_entry("PSETQS", OP_PSETQS),
  make_code_entry("VSETQ", OP_VSETQ),
  make_code_entry("VSETQC", OP_VSETQC),
  make_code_entry("VSETQS", OP_VSETQS),
  make_code_entry("BLOCK", OP_BLOCK),
  make_code_entry("DO", OP_DO),
  make_code_entry("CATCH", OP_CATCH),
  make_code_entry("FRAME", OP_FRAME),
  make_code_entry("TAGBODY", OP_TAGBODY),
  make_code_entry("EXIT_TAGBODY", OP_EXIT_TAGBODY),
  make_code_entry("EXIT_FRAME", OP_EXIT_FRAME),
  make_code_entry("PROTECT", OP_PROTECT),
  make_code_entry("PROTECT_NORMAL", OP_PROTECT_NORMAL),
  make_code_entry("PROTECT_EXIT", OP_PROTECT_EXIT),
  make_code_entry("PROGV", OP_PROGV),
  make_code_entry("EXIT_PROGV", OP_EXIT_PROGV),
  make_code_entry("PUSHVALUES", OP_PUSHVALUES),
  make_code_entry("POPVALUES", OP_POPVALUES),
  make_code_entry("PUSHMOREVALUES", OP_PUSHMOREVALUES),
  make_code_entry("VALUES", OP_VALUES),
  make_code_entry("VALUEREG0", OP_VALUEREG0),
  make_code_entry("NTHVAL", OP_NTHVAL),
  make_code_entry("NIL", OP_NIL),
  make_code_entry("NOT", OP_NOT),
  make_code_entry("PUSHNIL", OP_PUSHNIL),
  make_code_entry("CSET", OP_CSET),
  make_code_entry("STEPIN", OP_STEPIN),
  make_code_entry("STEPCALL", OP_STEPCALL),
  make_code_entry("STEPOUT", OP_STEPOUT),
  /* indirect words */
  make_dict_entry(".D", _nucl_word_print_dictionary),
  make_dict_entry(".S", _nucl_word_print_stack),
  make_dict_entry(".V", _nucl_word_print_values),
  make_dict_entry(".", _nucl_word_pop_and_print),
  NULL,
};

/* The "word" dispatcher (unlike a standard "bytecodes" dispatcher) does not
   open and close a new frame on invocation. We just s'lie.*/
cl_object
_nucl_word_dispatch(cl_narg narg, ...)
{
  const cl_env_ptr the_env = ecl_process_env();
  struct ecl_stack_frame aux_frame[1];
  cl_object frame = ecl_cast_ptr(cl_object, aux_frame);
  ecl_stack_frame_open(the_env, frame, 0);
  return ecl_interpret(frame, ECL_NIL, frame->frame.env->function);
}

/* -- Compilation ----------------------------------------------------------- */

static cl_object
nucl_compile_definition(void)
{
  /* The compilation (for now) is really trivial. When we encounter:

     - opcode -- insert this opcode directly                     | 1 opcode
     - symbol -- CALLW (takes value from data and calls it)      | 2 opcodes
     - object -- PUSHQ (takes value from data and push on stack) | 2 opcodes

     Opcodes are also represented here by symbols, but have a flag set that
     denotes that they may be threaded directly. Symbols denote functions that
     accept one argument, the symbol itself. Finally we put OP_EXIT. The
     compiled definition is then fed to ecl_interpret. */
  cl_object frame = nucl_stack_frame();
  cl_object word = nucl_alloc_bytecodes(), value;
  cl_index size = nucl_frame_size(frame), idx = 0, dat = 0;
  cl_index code_size = 2*size+1 ;      /* large enough, a bit wasteful */
  cl_opcode *code = ecl_alloc(code_size * sizeof(cl_opcode));
  cl_object data = ecl_make_stack(0);
  /* Compute the necessary code size */
  loop_across_frame_fifo(elt, frame) {
    switch(ecl_t_of(elt)) {
    case t_symbol:
      value = elt->symbol.value;
      if (ECL_FIXNUMP(value)) {
        code[idx++] = ecl_fixnum(value);
      } else if (value != OBJNULL) {
        code[idx++] = OP_CALLW;
        code[idx++] = dat++;
        ecl_stack_push(data, value);
      } else {
        nucl_write_cstr("$$$ error: invocation references an undefined word ");
        nucl_write_object(elt);
      }
      break;
    default:
      code[idx++] = OP_PUSHQ;
      code[idx++] = dat++;
      ecl_stack_push(data, elt);
    }
  } end_loop_across_frame();
  code[idx++] = OP_EXIT;
  nucl_stack_clear();
  word->bytecodes.code_size = code_size;
  word->bytecodes.code = ecl_cast_ptr(char*,code);
  word->bytecodes.data = data;
  word->bytecodes.entry = _nucl_word_dispatch;
  return word;
}

/* A word definition is a state machine:
 * 1. : starts the definition
 * 2. the first token  -> word name (symbol)
 * 3. remaining tokens -> word body
 * 4. ; ends the definition
 * Nested definitions are not allowed.
 */
cl_object nucl_compile_word(cl_object op) {
  cl_object definition_p = get_definition_p();
  if (Null(definition_p)) {
    set_definition_p(ECL_T);
    open_nucl_frame();
  } else if (definition_p == ECL_T) {
    (ecl_t_of(op) != t_symbol
      || !nucl_cstrcmp(op->symbol.name, ":", 1)
      || !nucl_cstrcmp(op->symbol.name, ";", 1))
      ? nucl_write_cstr("$$$ error: the word name must be a symbol\n")
      : set_definition_p(op);
  } else if (ecl_t_of(op) != t_symbol) {
    nucl_stack_push(op);
  } else if (!nucl_cstrcmp(op->symbol.name, ";", 1)) {
    cl_object definition = nucl_compile_definition();
    nucl_append_dictionary(definition_p, definition);
    close_nucl_frame();
    set_definition_p(ECL_NIL);
    return ECL_NIL;
  } else if (!nucl_cstrcmp(op->symbol.name, ":", 1)) {
    nucl_write_cstr("$$$ error: nested definitions are not allowed\n");
  } else {
    cl_object word = nucl_search_dictionary(op->symbol.name);
    (word == ECL_UNBOUND)
      ? nucl_write_cstr("$$$ error: definition references an undefined word\n")
      : nucl_stack_push(word);
  }
  return ECL_NIL;
}

void init_nucl_dictionary_entries()
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object dict = ECL_SYM_VAL(the_env, nucl_dt);
  cl_object *iterator = nucl_dictionary_default_entries;
  for(; *iterator != NULL; iterator++) {
    ecl_stack_push(dict, *iterator);
  }
}

cl_object nucl_process_token (cl_object token)
{
  if (ecl_t_of(token) != t_symbol) return token;
  return nucl_maybe_dictionary(token);
}

cl_object nucl_process_command ()
{
  cl_object result, command;
  if (Null(get_definition_p())) open_nucl_frame();

  result = limited_reader(ECL_CODE_CHAR('\n'));
  /* Processing of the command is straightforward. First we clean up the stack
     frame by moving it to a list, and then we process individual entries until.
     Complete definitions are not pushed back onto the stack, only words. */
  nucl_stack_to_list();
  command = nucl_stack_pop();
  /* First compile all definitions and push back other words onto the stack. */
  loop_for_on_unsafe(command) {
    cl_object elt = nucl_process_token(ECL_CONS_CAR(command));
    if(elt == _nucl_sym_def || elt == _nucl_sym_fed || !Null(get_definition_p()))
      nucl_compile_word(elt);
    else
      nucl_stack_push(elt);
  } end_loop_for_on_unsafe (command);
  /* Now if we are in not in a middle of the definition compile anonymous word
     from the current stack and call it. Otherwise leave the stack be. */
  if(Null(get_definition_p())) {
    const cl_env_ptr the_env = ecl_process_env();
    struct ecl_stack_frame aux_frame[1];
    cl_object frame = ecl_cast_ptr(cl_object, aux_frame);
    cl_object word = nucl_compile_definition();
    close_nucl_frame();
    ecl_stack_frame_open(the_env, frame, 0);
    ecl_interpret(frame, ECL_NIL, word);
  }
  return result;
}

void nucl_repl (void)
{
  const cl_env_ptr the_env = ecl_process_env();
  cl_object result, command;
  cl_index idx;
  init_nucl_io();
  init_nucl_reader();
  init_nucl_dictionary();
  init_nucl_dictionary_entries();
  do {
    if(Null(get_definition_p()))
      nucl_write_cstr("nucl> ");
    else
      nucl_write_cstr("... ");
    result = nucl_process_command();
    if(Null(get_definition_p()))
      nucl_write_cstr("... ok\n");
  } while(result != ECL_EOF);
  nucl_write_cstr("... bye\n");
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

  printf("Hello ECL! %p\n", the_env);
  nucl_repl();
  printf("Good bye ECL! %p\n", the_env);

  ecl_halt();
  return 0;
}
