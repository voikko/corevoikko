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

/* This module contains function to compile and execute pattern matching 
 * strings (regular expressions). */

/* Includes. ================================================================*/

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <glib.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/patterns.hpp"
#include "morphology/malaga/MalagaState.hpp"
#include "utf8/utf8.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

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

/* Functions. ===============================================================*/

bool 
match_pattern(string_t string, string_t pattern, MalagaState * malagaState)
/* Test whether STRING matches PATTERN and set substrings in PATTERN_VAR.
 * The substrings can be freed after usage. */
{
  struct {string_t start; string_t end;} var[ PATTERN_VAR_MAX ];
  int_t sp, i;
  bool found_mismatch;
  string_t index;

  sp = 0;
  found_mismatch = false;

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
	    free_mem( &(malagaState->pattern_var[i]) );
            malagaState->pattern_var[i] = new_string( var[i].start, var[i].end );
          }
        }
        return true;
      } 
      else 
	found_mismatch = true;
      break;
    case PAT_JUMP:
      pattern += (byte_t) pattern[1];
      break;
    case PAT_JUMP_NOW:
      if (sp == malagaState->stack_size)
      {
	malagaState->stack_size = renew_vector(&(malagaState->stack), sizeof( pattern_state_t ), 
				   malagaState->stack_size + 100 );
      }
      malagaState->stack[ sp ].string = string;
      malagaState->stack[ sp ].pattern = pattern + 2;
      sp++;
      pattern += (byte_t) pattern[1];
      break;
    case PAT_JUMP_LATER:
      if (sp == malagaState->stack_size)
      {
	malagaState->stack_size = renew_vector( &(malagaState->stack), sizeof( pattern_state_t ),
				   malagaState->stack_size + 100 );
      }
      malagaState->stack[ sp ].string = string;
      malagaState->stack[ sp ].pattern = pattern + (byte_t) pattern[1];
      sp++;
      pattern += 2;
      break;
    case PAT_MATCH_ANY:
      if (*string == EOS) 
	found_mismatch = true;
      else 
      {
	pattern++;
	utf8::unchecked::next(string);
      }
      break;
    case PAT_MATCH_CLASS:
      if (*string == EOS) 
	found_mismatch = true;
      else 
      { 
	index = pattern + 2;
	u_int_t c = utf8::unchecked::next(string);
        pattern += (byte_t) pattern[1] + 2;
        while (index < pattern && c != g_utf8_get_char( index ))
	  index = g_utf8_next_char( index );
        if (index >= pattern) 
	  found_mismatch = true;
      }
      break;
    case PAT_MATCH_NOT_CLASS:
      if (*string == EOS) 
	found_mismatch = true;
      else 
      { 
	index = pattern + 2;
	u_int_t c = utf8::unchecked::next(string);
        pattern += (byte_t) pattern[1] + 2;
        while (index < pattern && c != g_utf8_get_char( index ))
	  index = g_utf8_next_char( index );
        if (index < pattern) 
	  found_mismatch = true;
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
      if (*string == EOS) {
        found_mismatch = true;
      }
      else {
        u_int_t c = utf8::unchecked::next(string);
        if (c != utf8::unchecked::peek_next(pattern)) {
          found_mismatch = true;
        }
        else {
          utf8::unchecked::next(pattern);
        }
      }
      break;
    }

    /* If this path was not successful and there is another path, try it. */
    if (found_mismatch && sp > 0) 
    { 
      sp--;
      string = malagaState->stack[ sp ].string;
      pattern = malagaState->stack[ sp ].pattern;
      found_mismatch = false;
    }
  }
  return false;
}

/*---------------------------------------------------------------------------*/

void 
terminate_patterns(MalagaState * malagaState)
/* Terminate this module. */
{
  int_t i;

  for (i = 0; i < PATTERN_VAR_MAX; i++) 
    free_mem( &(malagaState->pattern_var[i]) );
  free_mem( &(malagaState->stack) );
  malagaState->stack_size = 0;
}

}}}
