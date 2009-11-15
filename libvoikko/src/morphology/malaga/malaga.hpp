/* Copyright (C) 1995 Bjoern Beutel
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

/* This is the header file for "libmalaga". */

/*===========================================================================*/

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Basic types. =============================================================*/

/* Numeric types. */

/** Unsigned 16 bits. */
typedef unsigned short int u_short_t;

/* Character types. */

/** A single byte of a char. */
typedef char char_t;

/** An EOS-terminated C-style string. */
typedef const char_t *string_t;

/** A value is stored in one or more cells.
 * Use this type if you want to allocate memory (pools etc.) for values. */ 
typedef u_short_t cell_t;

/** Reference to a Malaga values by this type. */
typedef cell_t *value_t;

typedef cell_t symbol_t;

/* Value functions. =========================================================*/

/** Return VALUE as a symbol. It is an error if VALUE is no symbol. */
extern symbol_t value_to_symbol(value_t value);

/** Return the value of STRING as a C-style string in external encoding. 
 * The string must be freed after use. */
extern char_t *get_value_string(value_t string);

/** Return the value of ATTRIBUTE in RECORD or NULL if it doesn't exist. */
extern value_t get_attribute(value_t record, symbol_t attribute);

/** Convert the STRING to a Malaga value and return it.
 * STRING must be a valid UTF-8 string.
 * The value must be freed after use.
 * This function sets "malaga_error". */
extern value_t parse_malaga_symbol(string_t string, MalagaState * malagaState);

/* Functions. ===============================================================*/

/** Analyse ITEM.
 * ITEM must be a valid UTF-8 string.
 * This function sets "malaga_error". */
extern void analyse_item(string_t item, MalagaState * malagaState);

/** Get the first result of the last call of "analyse_item".
 * Return NULL if there is no result. */
extern value_t first_analysis_result(MalagaState * malagaState);

/** Get the next result of the last call of "analyse_item".
 * Return NULL if there is no more result. */
extern value_t next_analysis_result(MalagaState * malagaState);

/** Initialise this module.
 * This function sets "malaga_error". */
extern void init_libmalaga(string_t project_directory, MalagaState * malagaState);

/** Terminate this module. */
extern void terminate_libmalaga(MalagaState * malagaState);

}}}
