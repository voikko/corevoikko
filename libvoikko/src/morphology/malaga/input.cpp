/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module supports reading and parsing input. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/input.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

static text_t *text; // FIXME

/* Functions. ===============================================================*/

void 
parse_whitespace( string_t *input )
/* Read whitespace in *INPUT and update *INPUT. */
{ 
  while (g_unichar_isspace( g_utf8_get_char( *input ) )) 
    *input = g_utf8_next_char( *input );
}

/*---------------------------------------------------------------------------*/

char_t * 
parse_word( string_t *input )
/* If there is a word in *INPUT, parse it up to the next space
 * and update *INPUT. Return the word. It must be freed after use.
 * If there's no word, report an error. */
{ 
  clear_text( text );
  if (**input == '\"') 
  { 
    /* A quoted word may contain spaces and quotes. */
    (*input)++;
    while (**input != '\"') 
    { 
      if ((*input)[0] == '\\' && (*input)[1] != EOS) 
	(*input)++;
      add_char_to_text( text, *(*input)++ );
    }
    (*input)++;
  } 
  else 
  { 
    while (**input != EOS)
    { 
      gunichar code = g_utf8_get_char( *input );
      if (g_unichar_isspace( code ))
	break;
      add_unichar_to_text( text, code );
      *input = g_utf8_next_char( *input );
    }
  }
  parse_whitespace( input );
  return new_string( text->buffer, NULL );
}

/*---------------------------------------------------------------------------*/

char_t *
parse_absolute_path( string_t *input, string_t relative_to )
/* Parse the next file name in *INPUT and update *INPUT.
 * Make the file name absolute (relative to RELATIVE_TO).
 * If there is no file name, report an error. */
{ 
  char_t * path;
  char_t *abs_path;

  path = parse_word( input );
  abs_path = absolute_path( path, relative_to );
  free_mem( &path );
  return abs_path;
}

/*---------------------------------------------------------------------------*/

char_t * 
read_line( FILE *stream )
/* Read user input from STREAM until eof or newline is met.
 * Return the result string (without final EOL or EOF). 
 * The string must be freed after use.
 * If EOF is initially met, return NULL. */
{ 
  int_t c;

  /* Read initial char to see if it's end of file. */
  c = getc( stream );
  if (c == EOF) 
    return NULL;

  /* There is some real result, read it in and return it. */
  clear_text( text );
  while (c != '\n' && c != EOF)
  { 
    ADD_CHAR_TO_TEXT( text, c );
    c = getc( stream );
  } 
  return new_string( text->buffer, NULL );
}

/*---------------------------------------------------------------------------*/

void 
cut_comment( char_t *line )
/* Cut a "#"-comment in LINE if there is one. 
 * The char "#" is ignored if it occurs within a double-quoted string. */
{ 
  while (*line != EOS)
  { 
    if (*line == '#') 
    { 
      *line = EOS; 
      return; 
    }
    if (*line == '\"') 
    { 
      /* Read over double-quoted string. */
      line++;
      while (*line != '\"') 
      { 
	if (*line == EOS) 
	  return;
	if (line[0] == '\\' && line[1] != EOS) 
	  line += 2; 
	else  
	  line++;
      }
      line++;
    } 
    else 
      line++;
  }
}

/*---------------------------------------------------------------------------*/

void 
init_input( void )
/* Initialise this module. */
{
  text = new_text();
}

/*---------------------------------------------------------------------------*/

void 
terminate_input( void )
/* Terminate this module. */
{
  free_text( &text );
}

}}}
