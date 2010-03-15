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

/* Includes. ================================================================*/

#include <stdio.h>
#include <string.h>
#include <time.h>
#include "setup/DictionaryException.hpp"
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/malaga_files.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

static const char_t malaga[] = "MALAGA"; /* Magic key. */

/* Functions. ===============================================================*/

void 
check_header( common_header_t *header, 
              int_t file_type, 
	      int_t min_code_version, int_t max_code_version )
/* Check if HEADER is of FILE_TYPE and 
 * between MIN_CODE_VERSION and MAX_CODE_VERSION.
 * FILE_NAME is needed for error messages. */
{ 
  if (memcmp( header->malaga, malaga, sizeof( char_t ) * MALAGA_LEN ) != NULL) {
    throw setup::DictionaryException("File is not a Malaga file.");
  }
  if (header->file_type != file_type) {
    throw setup::DictionaryException("File is of wrong file type.");
  }
  if (header->code_version < min_code_version) {
    throw setup::DictionaryException("File has too old code version.");
  }
  if (header->code_version > max_code_version) {
    throw setup::DictionaryException("File has too new code version.");
  }
}

}}}
