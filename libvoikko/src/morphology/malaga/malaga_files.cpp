/* Copyright (C) 1995 Bjoern Beutel. */

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
              string_t file_name, int_t file_type, 
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
