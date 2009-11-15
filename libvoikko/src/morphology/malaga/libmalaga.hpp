/* Copyright (C) 1997 Bjoern Beutel.
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

/* This module defines a Malaga library to analyse words and sentences. */

#include "setup/DictionaryException.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

extern void terminate_libmalaga( void );
/* Terminate this module. */

extern void analyse_item( string_t item ) throw(setup::DictionaryException);
/* Analyse ITEM.
 * This function sets "malaga_error". */

extern char_t *get_value_string( value_t string );
/* Return the value of STRING as a C-style string in external coding. 
 * The string must be freed after use. */

extern value_t parse_malaga_symbol( string_t string );
/* Convert STRING to a Malaga value and return it.
 * The value must be freed after use.
 * This function sets "malaga_error". */

}}}
