/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains structures and functions for the run-time lexicon. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

extern void init_lexicon( string_t file_name );
/* Initialise this module. Read lexicon from file FILE_NAME. */

extern void terminate_lexicon( void );
/* Terminate this module. */

extern void search_for_prefix( string_t string );
/* Search lexicon for prefixes of STRING in increasing length. 
 * The results are obtained by calling "get_next_prefix". */

extern bool get_next_prefix( string_t *string_p, value_t *feat );
/* Get the next lexicon entry that is a prefix of STRING. 
 * Return false iff no more entries exist.
 * If another entry exists, set *STRING_P to the remainder of STRING
 * and *FEAT to the feature structure assigned to the lexicon entry.
 * STRING must have been set by "search_for_prefix". */

}}}
