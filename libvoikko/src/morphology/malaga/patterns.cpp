/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module contains function to compile and execute pattern matching 
 * strings (regular expressions). */

/* Includes. ================================================================*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/patterns.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

#define SPECIAL_CHARS ".[]-^()*?+|\\" /* Characters with a special meaning. */

/* These are the instructions for matching a pattern.
 *
 * A pattern is a 0-terminated sequence of CHARs, defined as follows:
 * - P[] is the coded pattern string.
 * - PI is the current index into P[].
 * - PS[] is the stack of indexes into P[], used for backtracking.
 * - T[] is the text to be examined.
 * - TI is the current index into T[].
 * - TS[] is the stack of indexes into T[], used for backtracking.
 * - SI is the stack index, used for PIS[] and TIS[].
 * - VB[I] and VE[I] store beginning and end, resp., for variable no. I. */
enum 
{ 
  /* code 0 is EOS */
  PAT_JUMP = 1, /* PI += (byte_t) P[PI+1]; */
  PAT_JUMP_NOW, /* SI++; PS[SI] = PI+2; TS[SI] = TI; PI += (byte_t) P[PI+1]; */
  PAT_JUMP_LATER, /* SP++; PS[SI] = PI + (byte_t) P[PI]; TS[SI] = I; PI++; */
  PAT_MATCH_ANY, /* if T[TI] != EOS then TI++; else fail; */
  PAT_MATCH_CLASS, /* if (T[TI] in {P[ PI + 1 ],..,P[ PI + P[PI] ]})
                    * { TI++; PI += P[PI]+1; } else fail; */ 
  PAT_MATCH_NOT_CLASS, /* if (T[TI] in {P[ PI + 1 ],..,P[ PI + P[PI] ]})
			* fail; else { TI++; PI += P[PI]+1; } */
  PAT_START_VAR_0, /* VB[0] = I; PI++; */
  PAT_START_VAR_1, /* VB[1] = I; PI++; */
  PAT_START_VAR_2, /* VB[2] = I; PI++; */
  PAT_START_VAR_3, /* VB[3] = I; PI++; */
  PAT_START_VAR_4, /* VB[4] = I; PI++; */
  PAT_END_VAR_0, /* VE[0] = I; PI++; */
  PAT_END_VAR_1, /* VE[1] = I; PI++; */
  PAT_END_VAR_2, /* VE[2] = I; PI++; */
  PAT_END_VAR_3, /* VE[3] = I; PI++; */
  PAT_END_VAR_4, /* VE[4] = I; PI++; */
  PAT_COUNT
};

/* Types. ===================================================================*/

typedef struct {string_t string, pattern;} pattern_state_t;

/* Global variables. ========================================================*/

string_t pattern_var[ PATTERN_VAR_MAX ]; /* Pattern variables. */

/* Variables. ===============================================================*/

static pattern_state_t *stack; /* Stack used for backtracking. */
static int_t stack_size;

/* Forwards. ================================================================*/

static void compile_pattern_local( text_t *text, 
				   string_t *string_p, 
				   bool *may_be_empty );

/* Functions. ===============================================================*/

static bool 
is_pattern_char( string_t s )
{
  /* The following expression is true for each non-ASCII character. */
  return ((*s == '\\' && s[1] != EOS && strchr( SPECIAL_CHARS, s[1] ) != NULL)
          || (ORD(*s) >= PAT_COUNT && strchr( SPECIAL_CHARS, *s ) == NULL));
}

/*---------------------------------------------------------------------------*/

static gunichar
pattern_char( string_t *string_p )
/* See if *STRING_P points to a valid char or to an escape sequence.
 * Return the character code. Show an error if not valid. */
{
  string_t s;
  gunichar c;

  s = *string_p;
  if (s[0] == '\\' && s[1] != EOS && strchr( SPECIAL_CHARS, s[1] ) != NULL) 
  { 
    c = s[1];
    s += 2;
  }
  else if (ORD(*s) >= PAT_COUNT && strchr( SPECIAL_CHARS, s[0] ) == NULL)
  { 
    c = g_unichar_tolower( g_utf8_get_char( s ) );
    s = g_utf8_next_char( s );
  }
  else {
    malaga_throw();
  }
  *string_p = s;
  return c;
}

/*---------------------------------------------------------------------------*/

static char_t 
offset( int_t offs )
/* Return OFFS as a char. */
{
  return (byte_t) offs;
}

/*---------------------------------------------------------------------------*/

static void 
compile_char_class( text_t *text, string_t *string_p )
/* Compile a character class at *STRING_P.
 * Save the resulting pattern in TEXT. */
{
  string_t s;
  gunichar ca, ce, c; /* First char, last char and current char for ranges. */
  int_t patch_index;

  s = *string_p;
  s++;
  if (*s == '^') 
  { 
    s++;
    add_char_to_text( text, PAT_MATCH_NOT_CLASS );
  } 
  else 
    add_char_to_text( text, PAT_MATCH_CLASS );
  patch_index = text->string_size;

  /* Read chars and ranges. */
  do 
  { 
    ca = pattern_char( &s );
    if (*s == '-') 
    { 
      s++;
      ce = pattern_char( &s );
    } 
    else 
      ce = ca;
    for (c = ca; c <= ce; c++) 
      add_unichar_to_text( text, c );
  } while (*s != ']');
  *string_p = ++s;
  insert_char_in_text( text, offset( text->string_size - patch_index ), 
                       patch_index ); 
}

/*---------------------------------------------------------------------------*/

static bool
check_greedy( string_t *string_p )
{
  if (**string_p == '?')
  {
    (*string_p)++;
    return FALSE;
  }
  else 
    return TRUE;
}

/*---------------------------------------------------------------------------*/

static void 
compile_atom( text_t *text, string_t *string_p, bool *may_be_empty )
/* Compile an atom at *STRING_P. 
 * Save the resulting pattern in TEXT. 
 * MAY_BE_EMPTY becomes TRUE iff the atom may match an empty string. */
{
  string_t s;
  int_t start, length;
  bool may_be_empty_local, greedy;

  s = *string_p;
  *may_be_empty = TRUE;
  while (TRUE) 
  { 
    may_be_empty_local = FALSE;
    start = text->string_size;
    if (*s == '[') 
      compile_char_class( text, &s );
    else if (*s == '.') 
    { 
      s++;
      add_char_to_text( text, PAT_MATCH_ANY );
    } 
    else if (*s == '(') 
    { 
      s++;
      compile_pattern_local( text, &s, &may_be_empty_local );
    } 
    else if (is_pattern_char( s )) 
      add_unichar_to_text( text, pattern_char( &s ) );
    else 
      break;
    length = text->string_size - start;

    /* There may be a postfix operator. */
    if (*s == '?') 
    { 
      s++;
      greedy = check_greedy( &s );
      if (greedy) 
	insert_char_in_text( text, PAT_JUMP_LATER, start );
      else 
	insert_char_in_text( text, PAT_JUMP_NOW, start );
      insert_char_in_text( text, offset( 2 + length ), start + 1 );
    }
    else if (*s == '*') 
    { 
      s++;
      greedy = check_greedy( &s );
      insert_char_in_text( text, PAT_JUMP, start );
      insert_char_in_text( text, offset( 2 + length ), start + 1 );
      if (greedy) 
	add_char_to_text( text, PAT_JUMP_NOW );
      else 
	add_char_to_text( text, PAT_JUMP_LATER );
      add_char_to_text( text, offset( -length ) );
    } 
    else if (*s == '+') 
    { 
      s++;
      greedy = check_greedy( &s );
      if (greedy) 
	add_char_to_text( text, PAT_JUMP_NOW );
      else 
	add_char_to_text( text, PAT_JUMP_LATER );
      add_char_to_text( text, offset( -length ) );
      *may_be_empty = FALSE;
    } 
    else 
      *may_be_empty &= may_be_empty_local;
  }
  *string_p = s;
}

/*---------------------------------------------------------------------------*/

static void 
compile_pattern_local (text_t *text, string_t *string_p, bool *may_be_empty)
/* Convert STRING_P to a pattern to be used as input to "match_pattern".
 * If PATTERN_VAR_NO != -1, mark the pattern so the string matching this
 * pattern will be stored in PATTERN_VAR[ PATTERN_VAR_NO ].
 * The result pattern must be freed after usage. */
{
  string_t s;
  int_t start, length;
  bool may_be_empty_local;

  s = *string_p;
  start = text->string_size;
  compile_atom( text, &s, may_be_empty );
  length = text->string_size - start;
  while (*s == '|') 
  { 
    s++;

    /* Add jump from start of last alternative to start of this alternative. */
    insert_char_in_text( text, PAT_JUMP_LATER, start++ );
    insert_char_in_text( text, offset( length + 4 ), start++ );

    /* Compile this alternative. */
    start = text->string_size;
    compile_atom( text, &s, &may_be_empty_local );
    length = text->string_size - start;
    *may_be_empty |= may_be_empty_local;

    /* Add jump from end of last alternative to end of this alternative. */
    insert_char_in_text( text, PAT_JUMP, start++ );
    if (*s == '|') 
      insert_char_in_text( text, offset( length + 4 ), start++ ); 
    else
      insert_char_in_text( text, offset( length + 2 ), start++ );
  }
  *string_p = s;
}

/*---------------------------------------------------------------------------*/

string_t 
compile_pattern( string_t string, int_t pattern_var_no )
/* Convert STRING to a pattern to be used as input to "match_pattern".
 * If PATTERN_VAR_NO != -1, mark the pattern so the string matching this
 * pattern will be stored in PATTERN_VAR[ PATTERN_VAR_NO ].
 * The result pattern must be freed after usage. */
{
  text_t *text;
  bool may_be_empty;

  text = new_text();
  if (pattern_var_no != -1) 
    add_char_to_text( text, PAT_START_VAR_0 + pattern_var_no );
  compile_pattern_local( text, &string, &may_be_empty );
  if (pattern_var_no != -1) 
    add_char_to_text( text, PAT_END_VAR_0 + pattern_var_no );
  return text_to_string( &text );
}

/*---------------------------------------------------------------------------*/

bool 
match_pattern( string_t string, string_t pattern )
/* Test whether STRING matches PATTERN (a string of chars compiled with
 * "compile_pattern") and set substrings in PATTERN_VAR.
 * The substrings can be freed after usage. */
{
  struct {string_t start; string_t end;} var[ PATTERN_VAR_MAX ];
  int_t sp, i;
  bool found_mismatch;
  string_t index;      
  gunichar c;

  sp = 0;
  found_mismatch = FALSE;

  /* Clear all variables. */
  for (i = 0; i < PATTERN_VAR_MAX; i++) 
    var[i].start = var[i].end = NULL;

  while (! found_mismatch) 
  { 
    switch (*pattern) 
    {
    case EOS:
      if (*string == EOS) 
      { 
	for (i = 0; i < PATTERN_VAR_MAX; i++) 
	{ 
	  if (var[i].start != NULL && var[i].end != NULL) 
	  { 
	    free_mem( &pattern_var[i] );
            pattern_var[i] = new_string( var[i].start, var[i].end );
          }
        }
        return TRUE;
      } 
      else 
	found_mismatch = TRUE;
      break;
    case PAT_JUMP:
      pattern += (byte_t) pattern[1];
      break;
    case PAT_JUMP_NOW:
      if (sp == stack_size)
      {
	stack_size = renew_vector( &stack, sizeof( pattern_state_t ), 
				   stack_size + 100 );
      }
      stack[ sp ].string = string;
      stack[ sp ].pattern = pattern + 2;
      sp++;
      pattern += (byte_t) pattern[1];
      break;
    case PAT_JUMP_LATER:
      if (sp == stack_size)
      {
	stack_size = renew_vector( &stack, sizeof( pattern_state_t ),
				   stack_size + 100 );
      }
      stack[ sp ].string = string;
      stack[ sp ].pattern = pattern + (byte_t) pattern[1];
      sp++;
      pattern += 2;
      break;
    case PAT_MATCH_ANY:
      if (*string == EOS) 
	found_mismatch = TRUE;
      else 
      {
	pattern++;
	string = g_utf8_next_char( string );
      }
      break;
    case PAT_MATCH_CLASS:
      if (*string == EOS) 
	found_mismatch = TRUE;
      else 
      { 
	index = pattern + 2;
	c = g_unichar_tolower( g_utf8_get_char( string ) );
	string = g_utf8_next_char( string );
        pattern += (byte_t) pattern[1] + 2;
        while (index < pattern && c != g_utf8_get_char( index ))
	  index = g_utf8_next_char( index );
        if (index >= pattern) 
	  found_mismatch = TRUE;
      }
      break;
    case PAT_MATCH_NOT_CLASS:
      if (*string == EOS) 
	found_mismatch = TRUE;
      else 
      { 
	index = pattern + 2;
	c = g_unichar_tolower( g_utf8_get_char( string ) );
	string = g_utf8_next_char( string );
        pattern += (byte_t) pattern[1] + 2;
        while (index < pattern && c != g_utf8_get_char( index ))
	  index = g_utf8_next_char( index );
        if (index < pattern) 
	  found_mismatch = TRUE;
      }
      break;
    case PAT_START_VAR_0:
    case PAT_START_VAR_1:
    case PAT_START_VAR_2:
    case PAT_START_VAR_3:
    case PAT_START_VAR_4:
      var[ *pattern++ - PAT_START_VAR_0 ].start = string;
      break;
    case PAT_END_VAR_0:
    case PAT_END_VAR_1:
    case PAT_END_VAR_2:
    case PAT_END_VAR_3:
    case PAT_END_VAR_4:
      var[ *pattern++ - PAT_END_VAR_0 ].end = string;
      break;
    default:
      if (*string == EOS)
	found_mismatch = TRUE;
      else
      {
	c = g_unichar_tolower( g_utf8_get_char( string ) );
	string = g_utf8_next_char( string );
	if (c != g_utf8_get_char( pattern )) 
	  found_mismatch = TRUE;
	else
	  pattern = g_utf8_next_char( pattern );
      }
      break;
    }

    /* If this path was not successful and there is another path, try it. */
    if (found_mismatch && sp > 0) 
    { 
      sp--;
      string = stack[ sp ].string;
      pattern = stack[ sp ].pattern;
      found_mismatch = FALSE;
    }
  }
  return FALSE;
}

/*---------------------------------------------------------------------------*/

void 
terminate_patterns( void )
/* Terminate this module. */
{
  int_t i;

  for (i = 0; i < PATTERN_VAR_MAX; i++) 
    free_mem( &pattern_var[i] );
  free_mem( &stack );
  stack_size = 0;
}

}}}
