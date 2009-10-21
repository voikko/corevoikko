/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* Options for malaga and functions to start and terminate malaga. */

/* Includes. ================================================================*/

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <time.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/pools.hpp"
#include "morphology/malaga/values.hpp"
#include "morphology/malaga/input.hpp"
#include "morphology/malaga/rule_type.hpp"
#include "morphology/malaga/rules.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/analysis.hpp"
#include "morphology/malaga/symbols.hpp"
#include "morphology/malaga/lexicon.hpp"
#include "morphology/malaga/scanner.hpp"
#include "morphology/malaga/patterns.hpp"
#include "morphology/malaga/malaga_lib.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

static const char * const project_file = "voikko-fi_FI.pro";
static const char * const morphology_file = "voikko-fi_FI.mor";
static const char * const lexicon_file = "voikko-fi_FI.lex";
static const char * const symbol_file = "voikko-fi_FI.sym";

/* Functions. ===============================================================*/

static const char * const pathSeparator() {
#ifdef WIN32
  return "\\";
#else
  return "/";
#endif
}

/*---------------------------------------------------------------------------*/

static void 
read_project_file( string_t project_file )
/* Read the project file. */
{ 
  FILE *project_stream;
  char_t *project_line;
  string_t project_line_p, argument, include_file;

  project_stream = open_stream( project_file, "r" );
  while (true) 
  { 
    project_line = read_line( project_stream );
    if (project_line == NULL) {
      break;
    }
    cut_comment( project_line );
    project_line_p = project_line;
    
    if (*project_line_p != EOS) 
    {
      argument = NULL;
      {
	argument = parse_word( &project_line_p );
	if (strcmp( argument, "include:" ) == 0) 
	{ 
	  include_file = parse_absolute_path( &project_line_p, project_file );
	  read_project_file( include_file );
	  free_mem( &include_file );
	}
	free_mem( &argument );
      }
    }
    free_mem( &project_line );
  }
  close_stream( &project_stream, project_file );
}

/*---------------------------------------------------------------------------*/

static const char * const
binarySuffix() {
  union { char_t chars[4]; int_t integer; } format;

  format.integer = 0x12345678;
  if (sizeof( int_t ) != 4) {
    return "_c";
  }
  else if (format.chars[0] == 0x12 && format.chars[1] == 0x34
	   && format.chars[2] == 0x56 && format.chars[3] == 0x78) {
    return "_b";
  }
  else if (format.chars[0] == 0x78 && format.chars[1] == 0x56
	   && format.chars[2] == 0x34 && format.chars[3] == 0x12) {
     return "_l";
  }
  else {
    return "_c";
  }
}

/*---------------------------------------------------------------------------*/

void 
init_malaga(string_t directoryName)
/* Initialise this module. */
{ 

  init_input();

  char * fullProjectFile = concat_strings(directoryName, pathSeparator(), project_file, NULL);
  read_project_file(fullProjectFile);

  /* Init modules. */
  char * fullSymbolFile = concat_strings(directoryName, pathSeparator(), symbol_file, binarySuffix(), NULL);
  char * fullLexiconFile = concat_strings(directoryName, pathSeparator(), lexicon_file, binarySuffix(), NULL);
  char * fullMorphologyFile = concat_strings(directoryName, pathSeparator(), morphology_file, binarySuffix(), NULL);
  
  init_values();
  init_symbols(fullSymbolFile);
  init_lexicon(fullLexiconFile);
  init_scanner();
  init_analysis(fullMorphologyFile);
  
  free(fullProjectFile);
  free(fullSymbolFile);
  free(fullLexiconFile);
  free(fullMorphologyFile);
}

/*---------------------------------------------------------------------------*/

void 
terminate_malaga( void )
/* Terminate this module. */
{
  terminate_analysis();
  terminate_patterns();
  terminate_scanner();
  terminate_lexicon();
  terminate_symbols();
  terminate_values();
  terminate_input();
}

}}}
