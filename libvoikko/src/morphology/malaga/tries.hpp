/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module implements a static trie structure. */

/* Types. ===================================================================*/

typedef struct  /* A "trie_entry_t" associates KEY with CONTENT. */
{ 
  string_t key; /* The key of a trie entry is considered case insensitive. */
  int_t content;
} trie_entry_t;

/* Functions. ===============================================================*/

extern bool lookup_trie( int_t *trie, 
                           int_t *node_index, 
                           string_t *input, 
                           int_t *content );
/* Test if a prefix of *INPUT matches the node at *NODE_INDEX in TRIE.
 * If it does, return TRUE (else return FALSE) and:
 *   *CONTENT contains the associated content,
 *   *NODE contains the subnode for the matched input, and
 *   *INPUT points to the first char behind the prefix. */

/* End of file. =============================================================*/
