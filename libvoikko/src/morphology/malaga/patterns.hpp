/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains function to compile and execute pattern matching 
 * strings (regular expressions). */

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Functions. ===============================================================*/

extern bool match_pattern(string_t string, string_t pattern, MalagaState * malagaState);
/* Test whether STRING matches PATTERN and set substring indices in PATTERN_VAR.
 * The substrings remain valid until "compile_pattern" is called again. */

extern void terminate_patterns(MalagaState * malagaState);
/* Terminate this module. */

}}}
