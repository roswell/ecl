/*
 * UNICODE NAMES DATABASE
 *
 * auto-generated, do not edit! (see contrib/unicode/)
 */
#ifndef ECL_UCD_NAMES_H
#define ECL_UCD_NAMES_H 1

#define ECL_UCD_FIRST_PAIR 14824
#define ECL_UCD_TOTAL_PAIRS 52657
#define ECL_UCD_TOTAL_GROUPS 668
#define ECL_UCD_LARGEST_CHAR_NAME 88
#define ECL_UCD_TOTAL_NAMES 44870

typedef struct {
  unsigned char codes[6];
} ecl_ucd_names_pair_type;

typedef struct {
  int smallest, largest, pair_code;
} ecl_ucd_names_char_group;

typedef struct {
  unsigned char pair[2];
  unsigned char code[3];
} ecl_ucd_code_and_pair;

extern const ecl_ucd_names_pair_type ecl_ucd_names_pair[ECL_UCD_TOTAL_PAIRS];
extern const ecl_ucd_names_char_group ecl_ucd_names_char[ECL_UCD_TOTAL_GROUPS];
extern const char *ecl_ucd_names_word[ECL_UCD_FIRST_PAIR];
extern const ecl_ucd_code_and_pair ecl_ucd_sorted_pairs[ECL_UCD_TOTAL_NAMES];

#endif
