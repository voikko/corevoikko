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

/* This file contains basic types, macros and functions used everywhere. */

/* Includes. ================================================================*/

#include <cstdio>
#include <cstdlib>
#include <cstdarg>
#include <cstring>
#include "morphology/malaga/basic.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* List functions. ==========================================================*/

void 
clear_list( list_t *list )
/* Initialise LIST to be an list */
{ 
  list->first = list->last = NULL; 
}

/*---------------------------------------------------------------------------*/

void add_node( list_t *list, list_node_t *node, position_t position )
/* Add NODE to LIST.
 * If POSITION = LIST_START, add it at the start of the list;
 * If POSITION = LIST_END, add it at the end. */
{ 
  if (list->first == NULL) 
  {
    node->next = NULL;
    list->first = list->last = node;
  } 
  else if (position == LIST_START) 
  { 
    node->next = list->first;
    list->first = node;
  } 
  else /* position == LIST_END */
  { 
    node->next = NULL;
    list->last->next = node;
    list->last = node;
  }
}

/*---------------------------------------------------------------------------*/

void insert_node( list_t *list, list_node_t *node, list_node_t *prev )
/* Insert NODE in LIST, behind PREV.
 * If PREV == NULL, insert NODE at the beginning of the list. */
{ 
  if (prev == NULL) 
  {
    node->next = list->first;
    list->first = node;
  } 
  else 
  { 
    node->next = prev->next;
    prev->next = node;
  }
  if (node->next == NULL) 
    list->last = node;
}

/*---------------------------------------------------------------------------*/

void *
remove_first_node( list_t *list )
/* Remove the first node in LIST and return it.
 * Return NULL if LIST is empty. */
{
  list_node_t *node;
    
  if (list->first == NULL) 
    return NULL;
  node = list->first;
  list->first = node->next;
  if (node == list->last) 
    list->last = NULL;
  return node;
}

/*---------------------------------------------------------------------------*/

void
remove_node( list_t *list, list_node_t *node )
/* Remove NODE in LIST. */
{ 
  list_node_t *prev_node;

  if (list->first == node) 
  { 
    list->first = node->next;
    prev_node = NULL;
  } 
  else 
  { 
    prev_node = list->first;
    while (prev_node->next != node) 
      prev_node = prev_node->next;
    prev_node->next = node->next;
  }
  if (node->next == NULL) 
    list->last = prev_node;
}

/*---------------------------------------------------------------------------*/

void *
new_node( list_t *list, int_t size, position_t position )
/* Add a node of size SIZE to LIST.
 * If POSITION = LIST_START, add the element at the start of the list;
 * If POSITION = LIST_END, add the element at the end. 
 * Return the newly created node. */
{ 
  list_node_t *node;

  node = (list_node_t *) new_mem( size );
  add_node( list, node, position );
  return node;
}

/*---------------------------------------------------------------------------*/

void 
free_first_node( list_t *list )
/* Remove first node in LIST and free it. */
{ 
  list_node_t *node;

  node = (list_node_t *) remove_first_node( list );
  if (node != NULL) 
    free_mem( &node );
}

/* Memory functions. ========================================================*/

void *
new_mem( int_t item_size )
/* Allocate a memory block of ITEM_SIZE bytes, clear it and return it. */
{ 
  if (item_size == 0) 
    return NULL;
  return calloc( 1, item_size );
}

/*---------------------------------------------------------------------------*/

void *
new_vector( int_t item_size, size_t item_count )
/* Allocate a memory block to contain ITEM_COUNT items of size ITEM_SIZE,
 * clear it and return it. */
{ 
  if (item_size == 0 || item_count == 0) 
    return NULL;
  return calloc( item_count, item_size );
}

/*---------------------------------------------------------------------------*/

size_t 
renew_vector( void *block_p, int_t item_size, size_t item_count )
/* Realloc *BLOCK_P to contain ITEM_COUNT items of ITEM_SIZE bytes each.
 * Return ITEM_COUNT. */
{ 
  void *block;

  block = *((void **) block_p);
  block = realloc( block, item_count * item_size );
  *((void **) block_p) = block;
  return item_count;
}


/* Functions for text (indefinitely growing strings). =======================*/

text_t *
new_text( void )
/* Return a new text structure. */
{ 
  text_t *text;
  
  text = (text_t *) new_mem( sizeof( text_t ) );
  text->buffer_size = 100;
  text->buffer = (char_t *) new_vector( sizeof( char_t ), text->buffer_size + 1 );
  /* text->buffer[0] is set to EOS. */
  text->string_size = 0;

  return text;
}

/*---------------------------------------------------------------------------*/

void 
free_text( text_t **text_p )
/* Free the content of *TEXT_P. */
{ 
  if (*text_p != NULL) 
  { 
    free_mem( &(*text_p)->buffer );
    free_mem( text_p );
  }
}

/*---------------------------------------------------------------------------*/

void 
add_to_text( text_t *text, string_t string )
/* Add STRING to TEXT. */
{ 
  size_t string_len = strlen( string );
  if (text->buffer_size < text->string_size + string_len + 1) 
  { 
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
                                      2 * (text->string_size + string_len) );
  }
  strcpy( text->buffer + text->string_size, string );
  text->string_size += string_len;
}

/*---------------------------------------------------------------------------*/

void 
add_char_to_text( text_t *text, char_t character )
/* Add CHARACTER to TEXT. */
{ 
  if (text->buffer_size < text->string_size + 2)
  {
    text->buffer_size = renew_vector( &text->buffer, sizeof( char_t ), 
				      2 * (text->string_size + 1) );
  }
  text->buffer[ text->string_size++ ] = character;
  text->buffer[ text->string_size ] = EOS;
}

/*---------------------------------------------------------------------------*/

char_t * 
text_to_string( text_t **text_p )
/* Return content of *TEXT_P as a string and delete *TEXT_P.
 * The string must be freed after use. */
{
  char_t *string;

  string = new_string( (*text_p)->buffer, NULL );
  free_text( text_p );
  return string;
}

/* String functions. ========================================================*/

char_t * 
new_string( string_t string, string_t end )
/* Allocate memory and copy STRING into it.
 * If END != NULL, it marks the end of the string.
 * The result string must be freed after use. */
{
  char_t *new_str;
  char_t *new_str_p;

  if (end == NULL) 
    end = string + strlen( string );
  new_str = new_str_p = (char_t *) new_vector( sizeof( char_t ), end - string + 1 );
  while (string < end) 
    *new_str_p++ = *string++;
  *new_str_p = EOS;
  return new_str;
}

/*---------------------------------------------------------------------------*/

char_t *
concat_strings( string_t first_string, ... )
/* Concatenate a list of strings and return the result string.
 * Must have NULL-terminated list of strings as parameters.
 * The result string must be freed after use. */
{
  va_list args;
  size_t length;
  string_t next_string;
  char_t *string;
  char_t *string_p;

  /* Compute length of the result string. */
  va_start( args, first_string );
  length = strlen( first_string );
  for (next_string = va_arg( args, string_t ); 
       next_string != NULL;
       next_string = va_arg( args, string_t )) 
  {
    length += strlen( next_string ); 
  }
  va_end( args );

  /* Concatenate strings. */
  va_start( args, first_string );
  string = (char_t *) new_vector( sizeof( char_t ), length + 1 );
  strcpy( string, first_string );
  string_p = string + strlen( first_string );
  for (next_string = va_arg( args, string_t ); 
       next_string != NULL; 
       next_string = va_arg( args, string_t )) 
  {
    strcpy( string_p, next_string );
    string_p += strlen( next_string );
  }
  va_end( args );

  return string;
}

}}}
