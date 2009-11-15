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

/* This module administrates the name and atoms for each symbol. */

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Functions. ===============================================================*/

extern symbol_t find_symbol(string_t name, MalagaState * malagaState);
/* Find a symbol by NAME in the symbol table and return its code.
 * If there is no symbol NAME, report an error. */

extern void init_symbols(string_t file_name, MalagaState * malagaState);
/* Initialise this module; Read symbol table from file FILE_NAME. */

extern void terminate_symbols(MalagaState * malagaState);
/* Terminate this module. */

}}}
