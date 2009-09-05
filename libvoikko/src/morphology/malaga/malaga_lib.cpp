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
#include "basic.h"
#include "pools.h"
#include "values.h"
#include "input.h"
#include "rule_type.h"
#include "rules.h"
#include "files.h"
#include "analysis.h"
#include "symbols.h"
#include "lexicon.h"
#include "scanner.h"
#include "patterns.h"
#include "malaga_lib.h"

/* Global variables. ========================================================*/

bool auto_tree; /* TRUE if tree is shown automatically. */
bool auto_result; /* TRUE if result is shown automatically. */
bool result_as_list; /* TRUE if results will be combined into a list. */
text_t *grammar_info; /* Information about grammar. */

string_t result_format, unknown_format, error_format; 
/* Format strings for output. */

/* Variables. ===============================================================*/

static string_t morphology_file, syntax_file, lexicon_file;
static string_t symbol_file, extended_symbol_file;

static bool info_in_project_file;
/* Indicates whether we have read grammar info from the current project file.
 * Used to insert empty lines between grammar infos from different project
 * files. */

/* Functions. ===============================================================*/


static void 
read_project_file( string_t project_file )
/* Read the project file. */
{ 
  FILE *project_stream;
  char_t *project_line;
  string_t project_line_p, argument, include_file, extension;
  string_t *name_p;
  volatile int_t line_count;

  info_in_project_file = FALSE;
  project_stream = open_stream( project_file, "r" );
  line_count = 0;
  while (TRUE) 
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
	else if (strcmp( argument, "esym:" ) == 0) 
	{
	  extension = "esym";
	  name_p = &extended_symbol_file; 
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
	else if (strcmp( argument, "syn:" ) == 0) 
	{
	  extension = "syn";
	  name_p = &syntax_file; 
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
	  info_in_project_file = TRUE;
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
  info_in_project_file = FALSE;
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
  if (extended_symbol_file != NULL) 
    init_symbols( extended_symbol_file );
  else 
    init_symbols( symbol_file );
  init_lexicon( lexicon_file );
  init_scanner();
  init_analysis( morphology_file );

  /* Set options to default values. */
  error_format = new_string( "%l: %s: error: %e", NULL );
  result_format = new_string( "%l: %s: %f", NULL );
  unknown_format = new_string( "%l: %s: unknown", NULL );
  auto_tree = FALSE;
  auto_result = TRUE;
}

/*---------------------------------------------------------------------------*/

void 
terminate_malaga( void )
/* Terminate this module. */
{
  free_mem( &error_format );
  free_mem( &result_format );
  free_mem( &unknown_format );
  terminate_analysis();
  free_mem( &syntax_file );
  free_mem( &morphology_file );
  terminate_patterns();
  terminate_scanner();
  terminate_lexicon();
  free_mem( &lexicon_file );
  terminate_symbols();
  free_mem( &extended_symbol_file );
  free_mem( &symbol_file );
  terminate_values();
  free_text( &grammar_info );
  terminate_input();
}

/* End of file. =============================================================*/
