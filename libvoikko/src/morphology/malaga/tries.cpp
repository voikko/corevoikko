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

/* This module implements a static trie structure */

/* Includes. ================================================================*/

#include <cstdlib>
#include <cstdio>
#include <cstring>
#include "character/charset.hpp"
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/tries.hpp"
#include "utf8/utf8.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Macros. ==================================================================*/

#define ALIGN(addr, n) (((ptr_t) (addr) + (ptr_t) (n - 1)) & ~ (ptr_t) (n - 1))
/* Align ADDR to next multiple of N. */

/* Types. ===================================================================*/

typedef struct /* A node in a list of keys. */
{ 
  list_node_t *next;
  u_short_t key; /* Key for this node. */
  int_t subnode; /* Index of node for KEY in trie. */
  int_t content; /* Content associated with KEY. */
} key_node_t;

typedef struct /* A dynamic trie node. */
{ 
  string_t prefix; /* The prefix of this node. */
  int_t prefix_len; /* The length of PREFIX in bytes. */
  list_t key_list; /* List of keys. */
} trie_node_t;

/* The trie is a vector of int_t that contains compact trie nodes.
 * A compact trie node is int_t-aligned and looks as follows:
 *   u_byte_t prefix_len;
 *   char_t prefix[ prefix_len ];
 *   0 or 1 pad bytes of value 0;
 *   u_short_t subnode_count;
 *   u_short_t content_count;
 *   u_short_t subnode_keys[ subnode_count ];
 *   u_short_t content_keys[ content_count ];
 *   0 or 2 pad bytes of value 0;
 *   int_t subnodes[ subnode_count ];
 *   int_t contents[ content_count ]; */

typedef struct /* Pointers to the items of a compact trie node. */
{ 
  u_byte_t *prefix_len; /* Number of chars that precede the keys. */
  char_t *prefix; /* The chars that precede the keys. */ 
  u_short_t *subnode_count; /* The number of subnodes in this node. */
  u_short_t *content_count; /* The number of contents in this node. */
  u_short_t *subnode_keys; /* The keys for the subnodes in this node. */
  u_short_t *content_keys; /* The keys for the contents in this node. */
  int_t *subnodes; /* Indexes of the subnodes. */
  int_t *contents; /* Contents. */
} compact_node_t;

/*---------------------------------------------------------------------------*/

bool
lookup_trie( int_t *trie, int_t *node_index, string_t *input, int_t *content )
/* Test if a prefix of *INPUT matches the node at *NODE_INDEX in TRIE.
 * If it does, return true (else return false) and:
 *   *CONTENT contains the associated content,
 *   *NODE contains the subnode for the matched input, and
 *   *INPUT points to the first char behind the prefix. */
{
  int_t lower, upper, middle;
  compact_node_t r; /* Pointers to the items of the root node; */

  while (*node_index != -1) 
  { 
    /* Test if node's prefix matches the given key. */
    r.prefix_len = (u_byte_t *) (trie + *node_index);
    r.prefix = (char_t *) (r.prefix_len + 1);
    if (strncmp( *input, r.prefix, *r.prefix_len ) != 0) 
      return false;
    (*input) += *r.prefix_len;

    /* Get the rest of the node. */
    r.subnode_count = (u_short_t *) ALIGN(r.prefix + *r.prefix_len, 2);
    r.content_count = (u_short_t *) (r.subnode_count + 1);
    r.subnode_keys = (u_short_t *) (r.content_count + 1);
    r.content_keys = (u_short_t *) (r.subnode_keys + *r.subnode_count);
    r.subnodes = (int_t *) ALIGN( r.content_keys + *r.content_count, 4 );
    r.contents = (int_t *) (r.subnodes + *r.subnode_count);

    /* Perform binary search for subnode with given key. */
    wchar_t inputChar = utf8::unchecked::next(*input);
    wint_t c = simpleLower(inputChar);
    *node_index = -1;
    lower = 0;
    upper = *r.subnode_count - 1;
    while (lower <= upper) 
    { 
      middle = (lower + upper) / 2;
      if (c < r.subnode_keys[ middle ]) 
	upper = middle - 1;
      else if (c > r.subnode_keys[ middle ]) 
	lower = middle + 1;
      else /* This entry matches. */
      { 
	*node_index = r.subnodes[ middle ];
	break;
      }
    }

    /* Perform binary search for content with given key. */
    *content = -1;
    lower = 0;
    upper = *r.content_count - 1;
    while (lower <= upper) 
    { 
      middle = (lower + upper) / 2;
      if (c < r.content_keys[ middle ]) 
	upper = middle - 1;
      else if (c > r.content_keys[ middle ]) 
	lower = middle + 1;
      else /* This entry matches. */
      { 
        *content = r.contents[ middle ];
        break;
      }
    }

    if (*content != -1) 
      return true;
  }
  return false;
}

}}}
