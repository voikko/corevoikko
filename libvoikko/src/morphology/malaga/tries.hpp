/* Copyright (C) 1995 Bjoern Beutel.
 *               2009 Harri Pitkänen <hatapitk@iki.fi>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *********************************************************************************/

/* Description. =============================================================*/

/* This module implements a static trie structure. */

namespace libvoikko { namespace morphology { namespace malaga {

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
 * If it does, return true (else return false) and:
 *   *CONTENT contains the associated content,
 *   *NODE contains the subnode for the matched input, and
 *   *INPUT points to the first char behind the prefix. */

}}}
