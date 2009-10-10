/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains function to compile and execute pattern matching 
 * strings (regular expressions). */

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

enum {PATTERN_VAR_MAX = 5}; /* Maximum number of pattern variables. */

/* Variables. ===============================================================*/

extern string_t pattern_var[ PATTERN_VAR_MAX ]; /* Pattern variables. FIXME */

/* Functions. ===============================================================*/

extern string_t compile_pattern( string_t string, int_t pattern_var_no );
/* Convert STRING to a pattern to be used as input to "match_pattern".
 * If PATTERN_VAR_NO != -1, mark the pattern so the string matching this
 * pattern will be stored in PATTERN_VAR[ PATTERN_VAR_NO ].
 * The result pattern must be freed after usage. */

extern bool match_pattern( string_t string, string_t pattern );
/* Test whether STRING matches PATTERN (a string of chars compiled with
 * "compile_pattern") and set substring indices in PATTERN_VAR.
 * The substrings remain valid until "compile_pattern" is called again. */

extern void terminate_patterns( void );
/* Terminate this module. */

}}}
