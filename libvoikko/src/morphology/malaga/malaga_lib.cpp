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

/* Global variables. ========================================================*/

text_t *grammar_info; /* Information about grammar. */

/* Variables. ===============================================================*/

static string_t morphology_file, lexicon_file; // FIXME
static string_t symbol_file; // FIXME

/* Functions. ===============================================================*/

static string_t extension_start( string_t name )
/* Return a pointer to the start (the dot) of the extension in NAME,
 * or to the end of the string if there is no extension. */
{
  string_t s, t;
  
  s = NULL;
  for (t = name; *t != EOS; t++)
  {
    if (*t == '/') 
      s = NULL;
#ifdef WIN32
    else if (*t == '\\') 
      s = NULL;
#endif
    else if (*t == '.') 
      s = t;
  }
  return (s != NULL ? s : t);
}

/*---------------------------------------------------------------------------*/

static bool 
has_extension( string_t file_name, string_t extension )
/* Test if FILE_NAME has extension EXTENSION. */
{
  string_t ext; /* The real extension of FILE_NAME (including "."). */

  ext = extension_start( file_name );
  return (*ext != EOS && strcmp( ext + 1, extension ) == 0);
}

/*---------------------------------------------------------------------------*/

static void 
read_project_file( string_t project_file )
/* Read the project file. */
{ 
  FILE *project_stream;
  char_t *project_line;
  string_t project_line_p, argument, include_file, extension;
  string_t *name_p;
  volatile int_t line_count;

  bool info_in_project_file = false;
  project_stream = open_stream( project_file, "r" );
  line_count = 0;
  while (true) 
  { 
    project_line = read_line( project_stream );
    if (project_line == NULL) 
      break;
    line_count++;
    cut_comment( project_line );
    project_line_p = project_line;
    
    if (*project_line_p != EOS) 
    {
      argument = NULL;
      {
	argument = parse_word( &project_line_p );
	extension = NULL; 
	name_p = NULL;
	if (strcmp( argument, "sym:" ) == 0) 
	{
	  extension = "sym";
	  name_p = &symbol_file; 
	}
	else if (strcmp( argument, "lex:" ) == 0) 
	{
	  extension = "lex";
	  name_p = &lexicon_file; 
	}
	else if (strcmp( argument, "mor:" ) == 0) 
	{
	  extension = "mor";
	  name_p = &morphology_file; 
	}
	else if (strcmp( argument, "include:" ) == 0) 
	{ 
	  include_file = parse_absolute_path( &project_line_p, project_file );
	  read_project_file( include_file );
	  free_mem( &include_file );
	} 
	else if (strcmp( argument, "info:" ) == 0) 
        { 
	  /* Insert an empty line if we already have info that stems from a
	   * different project file. */
	  if (grammar_info->string_size > 0 && ! info_in_project_file)
	    add_char_to_text( grammar_info, '\n' );
	  add_to_text( grammar_info, project_line_p );
	  add_char_to_text( grammar_info, '\n' );
	  info_in_project_file = true;
	}
	free_mem( &argument );
      
	if (name_p != NULL && *name_p == NULL && *project_line_p != EOS) 
        { 
	  argument = parse_absolute_path( &project_line_p, project_file );
	  if (! has_extension( argument, extension ))
	  {
	    complain( "\"%s\" should have extension \"%s\".", 
		      name_in_path( argument ), extension );
	  }
	  set_binary_file_name( name_p, argument );
	  free_mem( &argument );
	}
      }
    }
    free_mem( &project_line );
  }
  close_stream( &project_stream, project_file );
  info_in_project_file = false;
}

/*---------------------------------------------------------------------------*/

void 
init_malaga( string_t project_file )
/* Initialise this module. */
{ 

  init_input();
  grammar_info = new_text();

  /* Read project file. */
  if (! has_extension( project_file, "pro" )) 
  {
    complain( "Project file \"%s\" must have extension \".pro\".", 
	      project_file );
  }
  read_project_file( project_file );
  if (morphology_file == NULL) 
    complain( "Missing morphology rules." );
  if (lexicon_file == NULL) 
    complain( "Missing lexicon." );
  if (symbol_file == NULL) 
    complain( "Missing symbol file." );

  /* Init modules. */
  init_values();
  init_symbols( symbol_file );
  init_lexicon( lexicon_file );
  init_scanner();
  init_analysis( morphology_file );
}

/*---------------------------------------------------------------------------*/

void 
terminate_malaga( void )
/* Terminate this module. */
{
  terminate_analysis();
  free_mem( &morphology_file );
  terminate_patterns();
  terminate_scanner();
  terminate_lexicon();
  free_mem( &lexicon_file );
  terminate_symbols();
  free_mem( &symbol_file );
  terminate_values();
  free_text( &grammar_info );
  terminate_input();
}

}}}
