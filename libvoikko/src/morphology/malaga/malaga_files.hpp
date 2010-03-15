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

/* This module defines the structure of compiled Malaga files. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_MALAGA_FILES_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_MALAGA_FILES_HPP

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

enum {MALAGA_LEN =  6}; /* Length of magic code at beginning of Malaga file. */

/* Values for FILE_TYPE. */
enum {SYMBOL_FILE, RULE_FILE, LEXICON_FILE, PRELEX_FILE};

/* Versions of compiled Malaga file types. */
enum {SYMBOL_CODE_VERSION = 8};
enum {RULE_CODE_VERSION = 47};
enum {LEXICON_CODE_VERSION = 14};
enum {PRELEX_CODE_VERSION = 1};

/* Versions of Malaga file types that are still understood. */
enum {MIN_SYMBOL_CODE_VERSION = 8};
enum {MIN_RULE_CODE_VERSION = 47};
enum {MIN_LEXICON_CODE_VERSION = 14};
enum {MIN_PRELEX_CODE_VERSION = 1};

/* Types. ===================================================================*/

typedef struct /* The common header of every Malaga file. */
{ 
  char_t malaga[ MALAGA_LEN ]; /* "MALAGA" to recognise Malaga files. */
  u_byte_t split_hangul_syllables; /* Boolean field. */
  byte_t file_type; /* SYMBOL_FILE, RULE_FILE or LEXICON_FILE. */
  int_t code_version; /* Only load code of the current version. */
  int_t sym_stamp; /* Stamp for ".sym" file. */
  int_t esym_stamp; /* Stamp for ".esym" file or 0. */
} common_header_t;

/*---------------------------------------------------------------------------*/

typedef struct /* The format of a Malaga rule file. */
{ 
  common_header_t common_header;
  int_t initial_rule_set; /* Index of the initial rule set in RULE_SETS. */
  int_t initial_feat; /* Index of the initial feature structure in VALUES. */
  int_t robust_rule; /* Rule number of robust_rule. */
  int_t pruning_rule; /* Rule number of pruning_rule. */
  int_t allo_rule; /* Rule number of allo_rule. */
  int_t input_filter; /* Rule number of input_filter. */
  int_t output_filter; /* Rule number of output_filter. */
  int_t rule_count; /* Number of rules in this file. */
  int_t rule_sets_size; /* Size of rule set table. */
  int_t instr_count; /* Number of instructions in this file. */
  int_t values_size; /* Size of Malaga value table. */
  int_t src_line_count; /* Number of correspondences
                         * between source lines and rule code. */
  int_t var_count; /* Number of variable names. */
  int_t var_scope_count; /* Number of variable scopes. */
  int_t constant_count; /* Number of named constants. */
  int_t strings_size; /* Size of string table. */
} rule_header_t;

/*---------------------------------------------------------------------------*/

typedef struct /* An entry in the symbol table. */ 
{ 
  int_t name; /* STRINGS index to symbol name. */ 
  int_t atoms; /* VALUES index to list of the atomic symbols
		* of a multi-symbol (or -1). */
} symbol_entry_t;

typedef struct /* The format of a Malaga symbol file. */
{ 
  common_header_t common_header;
  int_t symbol_count; /* Number of symbols in this file. */
  int_t values_size; /* Size of Malaga value table (for multi-symbols). */
  int_t strings_size; /* Size of string table (for symbol names). */

  /* The following blocks have dynamic size:
   * symbol_entry_t symbols[ symbol_count ];
   * cell_t values[ values_size ];
   * char_t strings[ strings_size ]; */
} symbol_header_t;

/*---------------------------------------------------------------------------*/

typedef struct /* An entry in the prelex file. */ 
{ 
  int_t surface; /* STRINGS index to surface. */ 
  int_t feat; /* VALUES index to feature structure. */
} prelex_entry_t;

typedef struct /* The format of a Malaga prelex file. */
{ 
  common_header_t common_header;
  int_t entry_count; /* Number of entries in this file. */
  int_t values_size; /* Size of value table. */
  int_t strings_size; /* Size of string table. */

  /* The following blocks have dynamic size:
   * prelex_entry_t entries[ entry_count ];
   * cell_t values[ values_size ];
   * char_t strings[ strings_size ]; */
} prelex_header_t;

/*---------------------------------------------------------------------------*/

typedef struct /* The format of a Malaga lexicon file. */
{ 
  common_header_t common_header;
  int_t trie_size; /* Size of trie table. */
  int_t trie_root; /* Index of root node in TRIE. */
  int_t feat_lists_size; /* Size of feature structure lists table. */
  int_t values_size; /* Size of value table. */

  /* The following blocks have dynamic size:
   * int_t trie[ trie_size ];
   * int_t feat_lists[ feat_lists_size ];
   * cell_t values[ values_size ]; */
} lexicon_header_t;

/* Functions. ===============================================================*/

extern void check_header( common_header_t *header, 
                          int_t file_type,
			  int_t min_code_version,
                          int_t max_code_version );
/* Check if HEADER is of FILE_TYPE and 
 * between MIN_CODE_VERSION and MAX_CODE_VERSION.
 * FILE_NAME is needed for error messages. */

}}}

#endif
