/* -*- Mode: C; c-basic-offset: 2; indent-tabs-mode: nil -*- */
/* vim: set filetype=c tabstop=2 shiftwidth=2 expandtab: */

/*
 * string.d - string routines
 *
 * Copyright (c) 1984 Taiichi Yuasa and Masami Hagiya
 * Copyright (c) 1990 Giuseppe Attardi
 * Copyright (c) 2001 Juan Jose Garcia Ripoll
 *
 * See file 'LICENSE' for the copyright details.
 *
 */

#include <ecl/ecl.h>
#include <string.h>
#include <ecl/internal.h>
#include <ecl/ecl-inl.h>

typedef ecl_character (*ecl_casefun)(ecl_character, bool *);

static cl_object
do_make_base_string(cl_index s, ecl_base_char code)
{
  cl_object x = ecl_alloc_simple_base_string(s);
  cl_index i;
  for (i = 0;  i < s;  i++)
    x->base_string.self[i] = code;
  return x;
}

#ifdef ECL_UNICODE
static cl_object
do_make_string(cl_index s, ecl_character code)
{
  cl_object x = ecl_alloc_simple_extended_string(s);
  cl_index i;
  for (i = 0;  i < s;  i++)
    x->string.self[i] = code;
  return x;
}
#else
#define do_make_string do_make_base_string
#endif

@(defun make_string (size &key (initial_element ECL_CODE_CHAR(' '))
                     (element_type @'character'))
  cl_index s;
  cl_object x;
  @
  s = ecl_to_index(size);
  /* INV: ecl_[base_]char_code() checks the type of initial_element() */
  if (element_type == @'base-char' || element_type == @'standard-char') {
    int code = ecl_base_char_code(initial_element);
    x = do_make_base_string(s, code);
  } else if (element_type == @'character') {
    cl_index code = ecl_char_code(initial_element);
    x = do_make_string(s, code);
  } else if (_ecl_funcall3(@'subtypep', element_type, @'base-char') == ECL_T) {
    int code = ecl_base_char_code(initial_element);
    x = do_make_base_string(s, code);
  } else if (_ecl_funcall3(@'subtypep', element_type, @'character') == ECL_T) {
    cl_index code = ecl_char_code(initial_element);
    x = do_make_string(s, code);
  } else {
    FEerror("The type ~S is not a valid string char type.",
            1, element_type);
  }
  @(return x);
  @)

/*
  Make a string of a certain size, with some leading zeros to
  keep C happy. The string must be adjustable, to allow further
  growth. (See unixfsys.c for its use).
*/
cl_object
ecl_alloc_adjustable_base_string(cl_index l)
{
  cl_object output = ecl_alloc_object(t_base_string);
  output->base_string.self       = (ecl_base_char *)ecl_alloc_atomic(l+1);
  output->base_string.self[l]    = 0;
  output->base_string.flags      = ECL_FLAG_HAS_FILL_POINTER | ECL_FLAG_ADJUSTABLE;
  output->base_string.elttype    = ecl_aet_bc;
  output->base_string.displaced  = ECL_NIL;
  output->base_string.dim        = l;
  output->base_string.fillp      = 0;
  return output;
}

#ifdef ECL_UNICODE
cl_object
ecl_alloc_adjustable_extended_string(cl_index l)
{
  cl_index bytes = sizeof(ecl_character) * l;
  cl_object output = ecl_alloc_object(t_string);
  output->string.self       = (ecl_character *)ecl_alloc_atomic(bytes);
  output->string.flags      = ECL_FLAG_HAS_FILL_POINTER | ECL_FLAG_ADJUSTABLE;
  output->string.elttype    = ecl_aet_ch;
  output->string.displaced  = ECL_NIL;
  output->string.dim        = l;
  output->string.fillp      = 0;
  return output;
}
#endif

/*
  ecl_make_simple_base_string(s) creates a simple-base string from C string s.
  ecl_make_constant_base_string(s) does the same, but without copying the C string.
*/
cl_object
ecl_make_constant_base_string(const char *s, cl_fixnum l)
{
  cl_object x = ecl_alloc_object(t_base_string);
  x->base_string.elttype = ecl_aet_bc;
  x->base_string.flags = 0; /* no fill pointer, not adjustable */
  x->base_string.displaced = ECL_NIL;
  if (l < 0) l = strlen(s);
  x->base_string.dim = (x->base_string.fillp = l);
  x->base_string.self = (ecl_base_char *)s;
  return x;
}

cl_object
ecl_make_simple_base_string(const char *s, cl_fixnum l)
{
  cl_object x;
  if (l < 0) l = strlen(s);

  x = ecl_alloc_simple_base_string(l);
  memcpy(x->base_string.self, s, l);
  return x;
}

cl_object
ecl_cstring_to_base_string_or_nil(const char *s)
{
  if (s == NULL)
    return ECL_NIL;
  else
    return ecl_make_simple_base_string(s,-1);
}

bool
ecl_fits_in_base_string(cl_object s)
{
  switch (ecl_t_of(s)) {
#ifdef ECL_UNICODE
  case t_string: {
    cl_index i;
    for (i = 0; i < s->string.fillp; i++) {
      if (!ECL_BASE_CHAR_CODE_P(s->string.self[i]))
        return 0;
    }
    return 1;
  }
#endif
  case t_base_string:
    return 1;
  default:
    FEwrong_type_nth_arg(@[si::copy-to-simple-base-string],1,s,@[string]);
  }
}

cl_object
si_copy_to_simple_base_string(cl_object x)
{
  cl_object y;
 AGAIN:
  switch(ecl_t_of(x)) {
  case t_symbol:
    x = x->symbol.name;
    goto AGAIN;
  case t_character:
    x = cl_string(x);
    goto AGAIN;
#ifdef ECL_UNICODE
  case t_string: {
    cl_index index, length = x->string.fillp;
    y = ecl_alloc_simple_base_string(length);
    for (index=0; index < length; index++) {
      ecl_character c = x->string.self[index];
      if (!ECL_BASE_CHAR_CODE_P(c))
        FEerror("Cannot coerce string ~A to a base-string", 1, x);
      y->base_string.self[index] = c;
    }
    break;
  }
#endif
  case t_base_string: {
    cl_index length = x->base_string.fillp;
    y = ecl_alloc_simple_base_string(length);
    memcpy(y->base_string.self, x->base_string.self, length);
    break;
  }
  case t_list:
    if (Null(x)) {
      x = ECL_NIL_SYMBOL->symbol.name;
      goto AGAIN;
    }
  default:
    FEwrong_type_nth_arg(@[si::copy-to-simple-base-string],1,x,@[string]);
  }
  @(return y);
}

cl_object
cl_string(cl_object x)
{
  switch (ecl_t_of(x)) {
  case t_symbol:
    x = x->symbol.name;
    break;
  case t_character: {
    cl_object y;
    ecl_character c = ECL_CHAR_CODE(x);
#ifdef ECL_UNICODE
    if (ECL_BASE_CHAR_CODE_P(c)) {
      y = ecl_alloc_simple_base_string(1);
      y->base_string.self[0] = c;
      x = y;
    } else {
      y = ecl_alloc_simple_extended_string(1);
      y->string.self[0] = c;
      x = y;
    }
#else
    y = ecl_alloc_simple_base_string(1);
    y->base_string.self[0] = c;
    x = y;
    break;
#endif
  }
#ifdef ECL_UNICODE
  case t_string:
#endif
  case t_base_string:
    break;
  case t_list:
    if (Null(x)) {
      x = ECL_NIL_SYMBOL->symbol.name;
      break;
    }
  default:
    FEwrong_type_only_arg(@[string],x,@[string]);
  }
  @(return x);
}

#ifdef ECL_UNICODE
cl_object
si_coerce_to_base_string(cl_object x)
{
  if (!ECL_BASE_STRING_P(x)) {
    x = si_copy_to_simple_base_string(x);
  }
  @(return x);
}

cl_object
si_coerce_to_extended_string(cl_object x)
{
  cl_object y;
 AGAIN:
  switch (ecl_t_of(x)) {
  case t_symbol:
    x = x->symbol.name;
    goto AGAIN;
  case t_character:
    y = ecl_alloc_simple_extended_string(1);
    y->string.self[0] = ECL_CHAR_CODE(x);
    break;
  case t_base_string: {
    cl_index index, len = x->base_string.dim;
    y = ecl_alloc_simple_extended_string(x->base_string.fillp);
    for(index=0; index < len; index++) {
      y->string.self[index] = x->base_string.self[index];
    }
    y->string.fillp = x->base_string.fillp;
  }
  case t_string:
    y = x;
    break;
  case t_list:
    if (Null(x)) {
      x = ECL_NIL_SYMBOL->symbol.name;
      goto AGAIN;
    }
  default:
    FEwrong_type_nth_arg(@[si::coerce-to-extended-string],1,x,@[string]);
  }
  @(return y);
}
#endif

cl_object
cl_char(cl_object object, cl_object index)
{
  cl_index position = ecl_to_index(index);
  @(return ECL_CODE_CHAR(ecl_char(object, position)));
}

ecl_character
ecl_char(cl_object object, cl_index index)
{
  /* CHAR bypasses fill pointers when accessing strings */
  switch(ecl_t_of(object)) {
#ifdef ECL_UNICODE
  case t_string:
    if (index >= object->string.dim)
      FEtype_error_index(object, index);
    return object->string.self[index];
#endif
  case t_base_string:
    if (index >= object->base_string.dim)
      FEtype_error_index(object, index);
    return object->base_string.self[index];
  default:
    FEwrong_type_nth_arg(@[char],1,object,@[string]);
  }
}

cl_object
si_char_set(cl_object object, cl_object index, cl_object value)
{
  cl_index position = ecl_to_index(index);
  cl_index c = ecl_char_code(value);
  ecl_char_set(object, position, c);
  @(return value);
}

ecl_character
ecl_char_set(cl_object object, cl_index index, ecl_character value)
{
  /* CHAR bypasses fill pointers when accessing strings */
  switch(ecl_t_of(object)) {
#ifdef ECL_UNICODE
  case t_string:
    if (index >= object->string.dim)
      FEtype_error_index(object, index);
    return object->string.self[index] = value;
#endif
  case t_base_string:
    if (index >= object->base_string.dim)
      FEtype_error_index(object, index);
    return object->base_string.self[index] = value;
  default:
    FEwrong_type_nth_arg(@[si::char-set],1,object,@[string]);
  }
}

#ifdef ECL_UNICODE
static int
compare_strings(cl_object string1, cl_index s1, cl_index e1,
                cl_object string2, cl_index s2, cl_index e2,
                int case_sensitive, cl_index *m)
{
  cl_index c1, c2;
  for (; s1 < e1; s1++, s2++) {
    if (s2 >= e2) { /* s1 is longer than s2, therefore s2 < s1 */
      *m = s1;
      return +1;
    }
    c1 = ecl_char(string1, s1);
    c2 = ecl_char(string2, s2);
    if (!case_sensitive) {
      c1 = ecl_char_upcase(c1);
      c2 = ecl_char_upcase(c2);
    }
    if (c1 < c2) {
      *m = s1;
      return -1;
    } else if (c1 > c2) {
      *m = s1;
      return +1;
    }
  }
  *m = s1;
  if (s2 >= e2) {
    return 0;
  } else { /* s1 is shorter than s2, hence s1 < s2 */
    return -1;
  }
}
#endif

static int
compare_base(unsigned char *s1, cl_index l1, unsigned char *s2, cl_index l2,
             int case_sensitive, cl_index *m)
{
  cl_index l, c1, c2;
  for (l = 0; l < l1; l++, s1++, s2++) {
    if (l == l2) { /* s1 is longer than s2, therefore s2 < s1 */
      *m = l;
      return +1;
    }
    c1 = *s1;
    c2 = *s2;
    if (!case_sensitive) {
      c1 = ecl_char_upcase(c1);
      c2 = ecl_char_upcase(c2);
    }
    if (c1 < c2) {
      *m = l;
      return -1;
    } else if (c1 > c2) {
      *m = l;
      return +1;
    }
  }
  *m = l;
  if (l1 == l2) 
    return 0;
  else { /* s1 is shorter than s2, hence s1 < s2 */
    return -1;
  }
}

@(defun string= (string1 string2 &key (start1 ecl_make_fixnum(0)) end1
                 (start2 ecl_make_fixnum(0)) end2)
  cl_index_pair p;
  cl_index s1, e1, s2, e2;
  @
  {
    string1 = cl_string(string1);
    string2 = cl_string(string2);
    p = ecl_vector_start_end(@[string=], string1, start1, end1);
    s1 = p.start; e1 = p.end;
    p = ecl_vector_start_end(@[string=], string2, start2, end2);
    s2 = p.start; e2 = p.end;
    if (e1 - s1 != e2 - s2) {
      @(return ECL_NIL);
    }
#ifdef ECL_UNICODE
    if (string1->string.t == t_string) {
      if (string2->string.t == t_string) {
        while (s1 < e1)
          if (string1->string.self[s1++] != string2->string.self[s2++]) {
            @(return ECL_NIL);
          }
        @(return ECL_T);
      } else {
        while (s1 < e1)
          if (string1->string.self[s1++] != string2->base_string.self[s2++]) {
            @(return ECL_NIL);
          }
        @(return ECL_T);
      }
    } else {
      if (string2->string.t == t_string) {
        while (s1 < e1)
          if (string1->base_string.self[s1++] != string2->string.self[s2++]) {
            @(return ECL_NIL);
          }
        @(return ECL_T);
      } else {
        while (s1 < e1)
          if (string1->base_string.self[s1++] != string2->base_string.self[s2++]) {
            @(return ECL_NIL);
          }
        @(return ECL_T);
      }
    }
#else
    while (s1 < e1)
      if (string1->base_string.self[s1++] != string2->base_string.self[s2++]) {
        @(return ECL_NIL);
      }
#endif
    @(return ECL_T);
  }
  @)

/*
  This correponds to string= (just the string equality).
*/
bool
ecl_string_eq(cl_object x, cl_object y)
{
  cl_index i, j;
  i = x->base_string.fillp;
  j = y->base_string.fillp;
  if (i != j) return 0;
#ifdef ECL_UNICODE
  switch(ecl_t_of(x)) {
  case t_string:
    switch(ecl_t_of(y)) {
    case t_string:
      return memcmp(x->string.self, y->string.self, i * sizeof *x->string.self) == 0;
    case t_base_string: {
      cl_index index;
      for(index=0; index<i; index++)
        if (x->string.self[index] != y->base_string.self[index])
          return 0;
      return 1;
    }
    default:
      FEwrong_type_nth_arg(@[string=],2,y,@[string]);
    }
    break;
  case t_base_string:
    switch(ecl_t_of(y)) {
    case t_string:
      return ecl_string_eq(y, x);
    case t_base_string:
      return memcmp(x->base_string.self, y->base_string.self, i) == 0;
    default:
      FEwrong_type_nth_arg(@[string=],2,y,@[string]);
    }
    break;
  default:
    FEwrong_type_nth_arg(@[string=],2,x,@[string]);
  }
#else
  return memcmp(x->base_string.self, y->base_string.self, i) == 0;
#endif
}


@(defun string_equal (string1 string2 &key (start1 ecl_make_fixnum(0)) end1
                      (start2 ecl_make_fixnum(0)) end2)
  cl_index s1, e1, s2, e2;
  cl_index_pair p;
  int output;
  @
  string1 = cl_string(string1);
  string2 = cl_string(string2);
  p = ecl_vector_start_end(@[string=], string1, start1, end1);
  s1 = p.start; e1 = p.end;
  p = ecl_vector_start_end(@[string=], string2, start2, end2);
  s2 = p.start; e2 = p.end;
  if (e1 - s1 != e2 - s2) {
    @(return ECL_NIL);
  }
#ifdef ECL_UNICODE
  if (ECL_EXTENDED_STRING_P(string1) || ECL_EXTENDED_STRING_P(string2)) {
    output = compare_strings(string1, s1, e1, string2, s2, e2, 0, &e1);
  } else
#endif
    output = compare_base(string1->base_string.self + s1, e1 - s1,
                          string2->base_string.self + s2, e2 - s2,
                          0, &e1);
  @(return ((output == 0)? ECL_T : ECL_NIL));
  @)

static cl_object
string_compare(cl_narg narg, int sign1, int sign2, int case_sensitive, ecl_va_list ARGS)
{
  cl_object string1 = ecl_va_arg(ARGS);
  cl_object string2 = ecl_va_arg(ARGS);
  cl_index s1, e1, s2, e2;
  cl_index_pair p;
  int output;
  cl_object result;
  cl_object KEYS[4];
#define start1 KEY_VARS[0]
#define end1 KEY_VARS[1]
#define start2 KEY_VARS[2]
#define end2 KEY_VARS[3]
#define start1p KEY_VARS[4]
#define start2p KEY_VARS[6]
  cl_object KEY_VARS[8];

  if (narg < 2) FEwrong_num_arguments_anonym();
  KEYS[0]=@':start1';
  KEYS[1]=@':end1';
  KEYS[2]=@':start2';
  KEYS[3]=@':end2';
  cl_parse_key(ARGS, 4, KEYS, KEY_VARS, NULL, FALSE);

  string1 = cl_string(string1);
  string2 = cl_string(string2);
  if (start1p == ECL_NIL) start1 = ecl_make_fixnum(0);
  if (start2p == ECL_NIL) start2 = ecl_make_fixnum(0);
  p = ecl_vector_start_end(@[string=], string1, start1, end1);
  s1 = p.start; e1 = p.end;
  p = ecl_vector_start_end(@[string=], string2, start2, end2);
  s2 = p.start; e2 = p.end;
#ifdef ECL_UNICODE
  if (ECL_EXTENDED_STRING_P(string1) || ECL_EXTENDED_STRING_P(string2)) {
    output = compare_strings(string1, s1, e1, string2, s2, e2,
                             case_sensitive, &e1);
  } else
#endif
    {
      output = compare_base(string1->base_string.self + s1, e1 - s1,
                            string2->base_string.self + s2, e2 - s2,
                            case_sensitive, &e1);
      e1 += s1;
    }
  if (output == sign1 || output == sign2) {
    result = ecl_make_fixnum(e1);
  } else {
    result = ECL_NIL;
  }
  @(return result);
#undef start1p
#undef start2p
#undef start1
#undef end1
#undef start2
#undef end2
    }

@(defun string< (&rest args)
  @
  @(return string_compare(narg, -1, -1, 1, args));
  @)

@(defun string> (&rest args)
  @
  @(return string_compare(narg, +1, +1, 1, args));
  @)

@(defun string<= (&rest args)
  @
  @(return string_compare(narg, -1, 0, 1, args));
  @)

@(defun string>= (&rest args)
  @
  @(return string_compare(narg, 0, +1, 1, args));
  @)

@(defun string/= (&rest args)
  @
  @(return string_compare(narg, -1, +1, 1, args));
  @)

@(defun string-lessp (&rest args)
  @
  @(return string_compare(narg, -1, -1, 0, args));
  @)

@(defun string-greaterp (&rest args)
  @
  @(return string_compare(narg, +1, +1, 0, args));
  @)

@(defun string-not-greaterp (&rest args)
  @
  @(return string_compare(narg, -1, 0, 0, args));
  @)

@(defun string-not-lessp (&rest args)
  @
  @(return string_compare(narg, 0, +1, 0, args));
  @)

@(defun string-not-equal (&rest args)
  @
  @(return string_compare(narg, -1, +1, 0, args));
  @)

bool
ecl_member_char(ecl_character c, cl_object char_bag)
{
  cl_index i, f;
  switch (ecl_t_of(char_bag)) {
  case t_list:
    loop_for_in(char_bag) {
      cl_object other = CAR(char_bag);
      if (ECL_CHARACTERP(other) && c == ECL_CHAR_CODE(other))
        return(TRUE);
    } end_loop_for_in;
    return(FALSE);
  case t_vector:
    for (i = 0, f = char_bag->vector.fillp;  i < f;  i++) {
      cl_object other = char_bag->vector.self.t[i];
      if (ECL_CHARACTERP(other) && c == ECL_CHAR_CODE(other))
        return(TRUE);
    }
    return(FALSE);
#ifdef ECL_UNICODE
  case t_string:
    for (i = 0, f = char_bag->string.fillp;  i < f;  i++) {
      if (c == char_bag->string.self[i])
        return(TRUE);
    }
    return(FALSE);
#endif
  case t_base_string:
    for (i = 0, f = char_bag->base_string.fillp;  i < f;  i++) {
      if (c == char_bag->base_string.self[i])
        return(TRUE);
    }
    return(FALSE);
  case t_bitvector:
    return(FALSE);
  default:
    FEwrong_type_nth_arg(@[member],2,char_bag,@[sequence]);
  }
}

static cl_object
string_trim0(bool left_trim, bool right_trim, cl_object char_bag, cl_object strng)
{
  cl_index i, j;

  strng = cl_string(strng);
  i = 0;
  j = ecl_length(strng);
  if (left_trim) {
    for (;  i < j;  i++) {
      cl_index c = ecl_char(strng, i);
      if (!ecl_member_char(c, char_bag))
        break;
    }
  }
  if (right_trim) {
    for (; j > i; j--) {
      cl_index c = ecl_char(strng, j-1);
      if (!ecl_member_char(c, char_bag)) {
        break;
      }
    }
  }
  return cl_subseq(3, strng, ecl_make_fixnum(i), ecl_make_fixnum(j));
}

cl_object
cl_string_trim(cl_object char_bag, cl_object strng)
{
  return string_trim0(TRUE, TRUE, char_bag, strng);
}

cl_object
cl_string_left_trim(cl_object char_bag, cl_object strng)
{
  return string_trim0(TRUE, FALSE, char_bag, strng);
}

cl_object
cl_string_right_trim(cl_object char_bag, cl_object strng)
{
  return string_trim0(FALSE, TRUE, char_bag, strng);
}

static cl_object
string_case(cl_narg narg, cl_object fun, ecl_casefun casefun, ecl_va_list ARGS)
{
  cl_object strng = ecl_va_arg(ARGS);
  cl_index_pair p;
  cl_index i;
  bool b;
  cl_object KEYS[2];
#define kstart KEY_VARS[0]
#define kend KEY_VARS[1]
#define kstartp KEY_VARS[2]
  cl_object KEY_VARS[4];

  if (narg < 1) FEwrong_num_arguments_anonym();
  KEYS[0]=@':start';
  KEYS[1]=@':end';
  cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

  strng = cl_string(strng);
  strng = cl_copy_seq(strng);
  if (kstartp == ECL_NIL)
    kstart = ecl_make_fixnum(0);
  p = ecl_vector_start_end(fun, strng, kstart, kend);
  b = TRUE;
#ifdef ECL_UNICODE
  if (ECL_EXTENDED_STRING_P(strng)) {
    for (i = p.start;  i < p.end;  i++)
      strng->string.self[i] = (*casefun)(strng->string.self[i], &b);
  } else
#endif
    for (i = p.start;  i < p.end;  i++)
      strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
  @(return strng);
#undef kstartp
#undef kstart
#undef kend
}

static ecl_character
char_upcase(ecl_character c, bool *bp)
{
  return ecl_char_upcase(c);
}

@(defun string-upcase (&rest args)
  @
  @(return string_case(narg, @[string-upcase], char_upcase, args));
  @)

static ecl_character
char_downcase(ecl_character c, bool *bp)
{
  return ecl_char_downcase(c);
}

@(defun string-downcase (&rest args)
  @
  @(return string_case(narg, @[string-downcase], char_downcase, args));
  @)

static ecl_character
char_capitalize(ecl_character c, bool *bp)
{
  if (ecl_lower_case_p(c)) {
    if (*bp)
      c = ecl_char_upcase(c);
    *bp = FALSE;
  } else if (ecl_upper_case_p(c)) {
    if (!*bp)
      c = ecl_char_downcase(c);
    *bp = FALSE;
  } else {
    *bp = !ecl_alphanumericp(c);
  }
  return c;
}

@(defun string-capitalize (&rest args)
  @
  @(return string_case(narg, @[string-capitalize], char_capitalize, args));
  @)


static cl_object
nstring_case(cl_narg narg, cl_object fun, ecl_casefun casefun, ecl_va_list ARGS)
{
  cl_object strng = ecl_va_arg(ARGS);
  cl_index_pair p;
  cl_index i;
  bool b;
  cl_object KEYS[2];
#define kstart KEY_VARS[0]
#define kend KEY_VARS[1]
#define kstartp KEY_VARS[2]
  cl_object KEY_VARS[4];

  if (narg < 1) FEwrong_num_arguments_anonym();
  KEYS[0]=@':start';
  KEYS[1]=@':end';
  cl_parse_key(ARGS, 2, KEYS, KEY_VARS, NULL, FALSE);

  if (ecl_unlikely(!ECL_STRINGP(strng)))
    FEwrong_type_nth_arg(fun, 1, strng, @[string]);
  if (kstartp == ECL_NIL)
    kstart = ecl_make_fixnum(0);
  p = ecl_vector_start_end(fun, strng, kstart, kend);
  b = TRUE;
#ifdef ECL_UNICODE
  if (ECL_EXTENDED_STRING_P(strng)) {
    for (i = p.start;  i < p.end;  i++)
      strng->string.self[i] = (*casefun)(strng->string.self[i], &b);
  } else
#endif
    for (i = p.start;  i < p.end;  i++)
      strng->base_string.self[i] = (*casefun)(strng->base_string.self[i], &b);
  @(return strng);
#undef kstartp
#undef kstart
#undef kend
}

@(defun nstring-upcase (&rest args)
  @
  @(return nstring_case(narg, @'nstring-upcase', char_upcase, args));
  @)

@(defun nstring-downcase (&rest args)
  @
  @(return nstring_case(narg, @'nstring-downcase', char_downcase, args));
  @)

@(defun nstring-capitalize (&rest args)
  @
  @(return nstring_case(narg, @'nstring-capitalize', char_capitalize, args));
  @)

@(defun si::base-string-concatenate (&rest args)
  cl_index l;
  int i;
  cl_object output;
  @
  /* Compute final size and store NONEMPTY coerced strings. */
  for (i = 0, l = 0; i < narg; i++) {
    cl_object s = si_coerce_to_base_string(ecl_va_arg(args));
    if (s->base_string.fillp) {
      ECL_STACK_PUSH(the_env, s);
      l += s->base_string.fillp;
    }
  }
  /* Do actual copying by recovering those strings */
  output = ecl_alloc_simple_base_string(l);
  while (l) {
    cl_object s = ECL_STACK_POP_UNSAFE(the_env);
    size_t bytes = s->base_string.fillp;
    l -= bytes;
    memcpy(output->base_string.self + l, s->base_string.self, bytes);
  }
  @(return output);
  @)

@(defun ext::octets-to-string (input &key
                               (external_format @':default')
                               (start ecl_make_fixnum(0))
                               (end ECL_NIL))
  cl_object output;
  cl_index input_size;
  cl_object input_stream;
  cl_index output_size;
  cl_object ret;
  @
  output = si_get_buffer_string();
  input_stream = si_make_sequence_input_stream(7, input,
                                               @':external-format', external_format,
                                               @':start', start,
                                               @':end', end);
  /* INV: MAKE-SEQUENCE-INPUT-STREAM checks types of start and end indices */
  input_size = (Null(end) ? ecl_length(input) : ecl_fixnum(end)) - ecl_fixnum(start);
  output_size = 0;
  do {
    output->base_string.fillp = output->base_string.dim;
    output_size += ecl_to_unsigned_integer(si_do_read_sequence(output, input_stream,
                                                               ecl_make_fixnum(output_size),
                                                               ecl_make_fixnum(output->base_string.dim)));
    if (output_size < output->base_string.dim) {
      break;
    }
    output = si_adjust_vector(output, ecl_make_fixnum(input_size > output_size
                                                      ? input_size
                                                      : output_size + 128));
  } while (1);
  output->base_string.fillp = output_size;
  if (ecl_fits_in_base_string(output)) {
    ret = si_copy_to_simple_base_string(output);
  } else {
    ret = cl_copy_seq(output);
  }
  si_put_buffer_string(output);
  @(return ret);
@)

@(defun ext::string-to-octets (input &key
                               (external_format @':default')
                               (start ecl_make_fixnum(0))
                               (end ECL_NIL)
                               (null_terminate ECL_NIL)
                               (element_type @'ext::byte8'))
  cl_object output;
  cl_object output_stream;
  cl_fixnum length;
  @
  length = (Null(end) ? ecl_length(input) : ecl_to_fixnum(end)) - ecl_to_fixnum(start) + 1;
  output = si_make_vector(element_type,       /* element-type */
                          ecl_make_fixnum(length), /* length */
                          ECL_T,              /* adjustable */
                          ecl_make_fixnum(0), /* fillp */
                          ECL_NIL,            /* displaced */
                          ECL_NIL);           /* displaced-offset */
  output_stream = si_make_sequence_output_stream(3, output,
                                                 @':external-format', external_format);
  si_do_write_sequence(input, output_stream, start, end);
  if (!Null(null_terminate)) {
    ecl_write_char(0, output_stream);
  }
  @(return output);
@)

cl_object
ecl_decode_from_cstring(const char *s, cl_fixnum len, cl_object encoding)
{
  volatile cl_object ret;
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), ecl_list1(@'ext::character-decoding-error')) {
    ret = si_octets_to_string(3, ecl_make_constant_base_string(s, len), @':external-format', encoding);
  } ECL_HANDLER_CASE(1, c) {
    ret = c; /* suppress "unused variable `c`" warning */
    ret = OBJNULL;
  } ECL_HANDLER_CASE_END;
  return ret;
}

cl_fixnum
ecl_encode_to_cstring(char *output, cl_fixnum output_len, cl_object input, cl_object encoding)
{
  volatile cl_fixnum ret;
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), ecl_list1(@'ext::character-encoding-error')) {
    cl_object output_vec = si_string_to_octets(3, input, @':external-format', encoding);
    ret = output_vec->vector.fillp + 1;
    if (ret <= output_len) {
      memcpy(output, output_vec->vector.self.b8, (ret-1)*sizeof(char));
      output[ret-1] = 0; /* null-terminator */
    }
  } ECL_HANDLER_CASE(1, c) {
    input = c; /* suppress "unused variable `c`" warning */
    ret = -1;
  } ECL_HANDLER_CASE_END;
  return ret;
}

#ifdef HAVE_WCHAR_H
cl_object
ecl_decode_from_unicode_wstring(const wchar_t *s, cl_fixnum len)
{
  cl_object input;
  cl_object elttype;
  cl_object encoding;
  volatile cl_object ret;
  if (len < 0) {
    len = wcslen(s);
  }
  switch (sizeof(wchar_t)) {
  case 1:
    elttype = @'ext::byte8';
    encoding = @':utf-8';
    break;
  case 2:
    elttype = @'ext::byte16';
    encoding = @':ucs-2';
    break;
  case 4:
    elttype = @'ext::byte32';
    encoding = @':ucs-4';
    break;
  default:
    ecl_internal_error("Unexpected sizeof(wchar_t)");
  }
  input = si_make_vector(elttype, ecl_make_fixnum(len), ECL_NIL, ECL_NIL, ECL_NIL, ECL_NIL);
  memcpy(input->vector.self.b8, s, len*sizeof(wchar_t));
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), ecl_list1(@'ext::character-decoding-error')) {
    ret = si_octets_to_string(3, input, @':external-format', encoding);
  } ECL_HANDLER_CASE(1, c) {
    ret = c; /* suppress "unused variable `c`" warning */
    ret = OBJNULL;
  } ECL_HANDLER_CASE_END;
  return ret;
}

cl_fixnum
ecl_encode_to_unicode_wstring(wchar_t *output, cl_fixnum output_len, cl_object input)
{
  cl_object elttype;
  cl_object encoding;
  volatile cl_fixnum ret;
  switch (sizeof(wchar_t)) {
  case 1:
    elttype = @'ext::byte8';
    encoding = @':utf-8';
    break;
  case 2:
    elttype = @'ext::byte16';
    encoding = @':ucs-2';
    break;
  case 4:
    elttype = @'ext::byte32';
    encoding = @':ucs-4';
    break;
  default:
    ecl_internal_error("Unexpected sizeof(wchar_t)");
  }
  ECL_HANDLER_CASE_BEGIN(ecl_process_env(), ecl_list1(@'ext::character-encoding-error')) {
    cl_object output_vec = si_string_to_octets(5, input, @':external-format', encoding, @':element-type', elttype);
    ret = output_vec->vector.fillp + 1;
    if (ret <= output_len) {
      memcpy(output, output_vec->vector.self.b8, (ret-1)*sizeof(wchar_t));
      output[ret-1] = 0; /* null-terminator */
    }
  } ECL_HANDLER_CASE(1, c) {
    input = c; /* suppress "unused variable `c`" warning */
    ret = -1;
  } ECL_HANDLER_CASE_END;
  return ret;
}
#endif
