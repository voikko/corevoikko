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

extern bool match_pattern( string_t string, string_t pattern );
/* Test whether STRING matches PATTERN and set substring indices in PATTERN_VAR.
 * The substrings remain valid until "compile_pattern" is called again. */

extern void terminate_patterns( void );
/* Terminate this module. */

}}}
