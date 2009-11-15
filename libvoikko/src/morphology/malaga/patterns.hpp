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

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Functions. ===============================================================*/

extern bool match_pattern(string_t string, string_t pattern, MalagaState * malagaState);
/* Test whether STRING matches PATTERN and set substring indices in PATTERN_VAR.
 * The substrings remain valid until "compile_pattern" is called again. */

extern void terminate_patterns(MalagaState * malagaState);
/* Terminate this module. */

}}}
