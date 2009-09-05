/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines the structure of compiled Malaga files. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <string.h>
#include <time.h>
#include <setjmp.h>
#include "basic.h"
#include "files.h"
#include "malaga_files.h"

/* Constants. ===============================================================*/

static char_t malaga[] = "MALAGA"; /* Magic key. */

/* Variables. ===============================================================*/

static time_t sym_stamp; /* Stamp for the ".sym" file or 0. */
static time_t esym_stamp; /* Stamp for the ".esym" file or 0. */

/* Functions. ===============================================================*/

void 
check_header( common_header_t *header, 
              string_t file_name, int_t file_type, 
	      int_t min_code_version, int_t max_code_version )
/* Check if HEADER is of FILE_TYPE and 
 * between MIN_CODE_VERSION and MAX_CODE_VERSION.
 * FILE_NAME is needed for error messages. */
{ 
  if (memcmp( header->malaga, malaga, sizeof( char_t ) * MALAGA_LEN ) != NULL) 
    complain( "\"%s\" is not a Malaga file.", file_name );
  if (header->file_type != file_type) 
    complain( "\"%s\" is wrong file type.", file_name );
  if (header->code_version < min_code_version) 
    complain( "\"%s\" is old code version. Maybe recompile?", file_name );
  if (header->code_version > max_code_version)
  {
    complain( "\"%s\" is new code version. Use newer Malaga version.", 
	      file_name);
  }
  if (file_type == SYMBOL_FILE) 
  { 
    sym_stamp = header->sym_stamp;
    esym_stamp = header->esym_stamp;
  } 
  else if (header->sym_stamp != sym_stamp) 
  { 
    complain( "\"%s\" uses %s \".sym\" file.", file_name,
	      header->sym_stamp < sym_stamp ? "older" : "newer" );
  } 
  else if (header->esym_stamp != 0) 
  { 
    if (esym_stamp == 0) 
      complain( "\"%s\" needs \".esym\" file.", file_name );
    else if (header->esym_stamp != esym_stamp) 
    {
      complain( "\"%s\" uses %s \".esym\" file.", file_name,
		header->esym_stamp < esym_stamp ? "older" : "newer" );
    }
  } 
}

/* End of file. =============================================================*/
