/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains structures and functions for the run-time lexicon. */

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

typedef struct {
    int_t feat_list_index;
    int_t trie_node;
    string_t prefix_end;
} trie_search_state;

/* Functions. ===============================================================*/

extern void init_lexicon(string_t file_name, MalagaState * malagaState);
/* Initialise this module. Read lexicon from file FILE_NAME. */

extern void terminate_lexicon(MalagaState * malagaState);
/* Terminate this module. */

extern void search_for_prefix(string_t string, trie_search_state & state, MalagaState * malagaState);
/* Search lexicon for prefixes of STRING in increasing length. 
 * The results are obtained by calling "get_next_prefix". */

extern bool get_next_prefix(string_t *string_p, value_t *feat, trie_search_state & state, MalagaState * malagaState);
/* Get the next lexicon entry that is a prefix of STRING. 
 * Return false iff no more entries exist.
 * If another entry exists, set *STRING_P to the remainder of STRING
 * and *FEAT to the feature structure assigned to the lexicon entry.
 * STRING must have been set by "search_for_prefix". */

}}}
