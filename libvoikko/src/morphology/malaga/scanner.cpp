/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module supports scanning (lexical analysis) of malaga source files. */

/* Includes. ================================================================*/

#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include "morphology/malaga/basic.hpp"
#include "morphology/malaga/files.hpp"
#include "morphology/malaga/scanner.hpp"

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

/* List of all keywords and their token codes.
 * (This list must be maintained in alphabetical order.) */
static const struct { string_t name; int_t code; } keywords[ NUMBER_OF_KEYWORDS ] = 
{ 
  { "accept", TOK_ACCEPT },
  { "allo_rule", TOK_ALLO_RULE },
  { "and", TOK_AND },
  { "assert", TOK_ASSERT },
  { "break", TOK_BREAK },
  { "choose", TOK_CHOOSE },
  { "combi_rule", TOK_COMBI_RULE },
  { "continue", TOK_CONTINUE },
  { "default", TOK_DEFAULT },
  { "define", TOK_DEFINE },
  { "else", TOK_ELSE },
  { "elseif", TOK_ELSEIF },
  { "end", TOK_END },
  { "end_rule", TOK_END_RULE },
  { "error", TOK_ERROR },
  { "foreach", TOK_FOREACH },
  { "greater", TOK_GREATER },
  { "greater_equal", TOK_GREATER_EQUAL },
  { "if", TOK_IF },
  { "in", TOK_IN },
  { "include", TOK_INCLUDE },
  { "initial", TOK_INITIAL },
  { "input_filter", TOK_INPUT_FILTER },
  { "less", TOK_LESS },
  { "less_equal", TOK_LESS_EQUAL },
  { "matches", TOK_MATCHES },
  { "not", TOK_NOT },
  { "or", TOK_OR },
  { "output_filter", TOK_OUTPUT_FILTER },
  { "parallel", TOK_PARALLEL },
  { "pruning_rule", TOK_PRUNING_RULE },
  { "repeat", TOK_REPEAT },
  { "require", TOK_REQUIRE },
  { "result", TOK_RESULT },
  { "return", TOK_RETURN },
  { "robust_rule", TOK_ROBUST_RULE },
  { "rules", TOK_RULES },
  { "select", TOK_SELECT },
  { "stop", TOK_STOP },
  { "subrule", TOK_SUBRULE },
  { "then", TOK_THEN },
  { "while", TOK_WHILE }
};

/* Types. ===================================================================*/

typedef struct /* A source stream for lexical analysis. */
{ 
  list_node_t *next; /* The next (including) source stream. */
  FILE *stream; /* The input stream for this include level. */
  string_t file_name; /* The name of the input file. */
  text_t *line; /* The current line. */
  string_t next_char_p; /* Pointer to the next char in LINE to be read. */
  int_t column; /* Column that has been read. */
  int_t line_number; /* Number of the line that has been read. */
  int_t next_char; /* Buffer NEXT_CHAR if this source is backed up. */
  int_t next_token; /* Buffer NEXT_TOKEN if this source is backed up. */
} source_t;

/* Global variables. ========================================================*/

int_t next_token;
string_t token_name;
char_t *token_string;
double token_number;

/* Variables. ===============================================================*/

static list_t sources; /* The list of sources, current source first. FIXME */

static int_t next_char; /* The next unicode char to be read. FIXME */

static text_t *token_text; /* The text of the next token. FIXME */

/* Functions. ===============================================================*/

static void 
read_next_char( string_t & scanner_input )
/* Read the next char from input into NEXT_CHAR.
 * If end of input stream is reached, return EOF.
 * If no input stream is selected, read input from INPUT_BUFFER.
 * If reading from stream, update column information. */
{ 
  source_t *source;
  int_t c;

  source = (source_t *) sources.first;
  if (scanner_input != NULL) /* Read from a string. */
  { 
    if (*scanner_input == EOS)
      next_char = EOF;
    else
    {
      next_char = g_utf8_get_char( scanner_input );
      scanner_input = g_utf8_next_char( scanner_input );
    }
  } 
  else if (source != NULL) /* Read from a file. */
  { 
    /* Read a new line if current line is empty. */
    if (*source->next_char_p == EOS)
    {
      clear_text( source->line );
      do
      {
	c = getc( source->stream );
	if (c == EOS)
	  complain( "Null byte in \"%s\"", source->file_name );
	else if (c == EOF) 
	{
	  if (ferror( source->stream ))
	  {
	    complain( "Can't read from \"%s\": %s.", 
		      source->file_name, strerror( errno ) );
	  }
	  else 
	    break;
	}
	else
	  ADD_CHAR_TO_TEXT( source->line, c );
      } while (c != '\n');
      
      if (! g_utf8_validate( source->line->buffer, -1, NULL ))
	complain( "Illegal UTF-8 character in \"%s\".", source->file_name );
      source->next_char_p = source->line->buffer;
    }

    if (*source->next_char_p == EOS)
      next_char = EOF;
    else
    {
      /* Get next char from current line. */
      next_char = g_utf8_get_char( source->next_char_p );
      source->next_char_p = g_utf8_next_char( source->next_char_p );
      
      /* Update line and column information. */
      if (next_char == '\t') 
	source->column = (source->column + 8) & ~7;
      else if (next_char == '\n') 
      { 
	source->column = 0;
	source->line_number++;
      }
      else if (next_char == '\r')
	source->column = 0;
      else if (next_char != EOF) 
	source->column++;
    }
  } 
  else 
    next_char = EOF;
}

/*---------------------------------------------------------------------------*/

string_t 
set_scanner_input( const string_t input )
/* Make the scanner use INPUT as scanner input 
 * until "set_scanner_input( NULL )" is called.
 * INPUT must remain valid until then. */
{
  source_t *source;

  source = (source_t *) sources.first;
  string_t scanner_input = input;
  if (input != NULL) 
  { 
    if (source != NULL) 
    { 
      source->next_char = next_char;
      source->next_token = next_token;
    }
    read_next_char(scanner_input);
    read_next_token(scanner_input);
  } 
  else if (source != NULL) 
  { 
    next_char = source->next_char;
    next_token = source->next_token;
  }
  return scanner_input;
}

/*---------------------------------------------------------------------------*/

void 
init_scanner( void )
/* Initialise the scanner. */
{
  token_text = new_text();
}

/*---------------------------------------------------------------------------*/

void 
terminate_scanner( void )
/* Terminate the scanner, even when it's scanning. */
{
  source_t *source;

  FOREACH_FREE( source, sources, source_t ) 
  {
    close_stream( &source->stream, NULL );
    free_text( &source->line );
  }
  token_name = NULL;
  free_text( &token_text );
  free_mem( &token_string );
}

/*---------------------------------------------------------------------------*/

static void 
read_name(string_t & scanner_input)
/* Read rule name, variable, or keyword into TOKEN_NAME. */
{
  token_name = NULL;
  clear_text( token_text );

  while (next_char != EOF
         && (g_unichar_isalnum( next_char )
	     || next_char == '_' || next_char == '&' || next_char == '|'))
  { 
    add_unichar_to_text( token_text, next_char );
    read_next_char(scanner_input);
  }

  token_name = token_text->buffer;
  if (*token_name == EOS) 
    complain( "Illegal character in name." );
}

/*---------------------------------------------------------------------------*/

static int_t 
keyword_code( string_t name )
/* Look up NAME in the keyword table and return its token value.
 * If NAME is no keyword, return TOK_IDENT. */
{
  int_t lower, upper, middle, result;

  /* We do a binary search on the keywords.
   * A keyword must be in the range of keywords[ lower..upper ]. */
  lower = 0;
  upper = NUMBER_OF_KEYWORDS - 1;
  while (lower <= upper) 
  { 
    middle = (lower + upper) / 2;
    result = strcmp( name, keywords[ middle ].name );
    if (result < 0) 
      upper = middle - 1;
    else if (result > 0) 
      lower = middle + 1;
    else 
      return keywords[ middle ].code;
  }
  return TOK_IDENT;
}

/*---------------------------------------------------------------------------*/

static void 
read_number(string_t & scanner_input)
/* Read a floating point number. Save its value in TOKEN_NUMBER. */
{
  token_name = NULL;
  clear_text( token_text );

  while (next_char >= '0' && next_char <= '9') 
  { 
    add_char_to_text( token_text, next_char );
    read_next_char(scanner_input);
  }
  if (next_char == 'l' || next_char == 'L') 
    read_next_char(scanner_input);
  else if (next_char == 'r' || next_char == 'R') 
  { 
    insert_char_in_text( token_text, '-', 0 );
    read_next_char(scanner_input);
  } 
  else 
  { 
    if (next_char == '.') 
    { 
      add_char_to_text( token_text, next_char );
      read_next_char(scanner_input);
      if (next_char < '0' || next_char >'9') 
	complain( "Missing digits after \".\"." );
      while (next_char >= '0' && next_char <= '9') 
      { 
	add_char_to_text( token_text, next_char );
        read_next_char(scanner_input);
      }
    }
    if (next_char == 'E' || next_char == 'e') 
    { /* Read an exponent. */
      add_char_to_text( token_text, next_char );
      read_next_char(scanner_input);
      if (next_char == '-' || next_char == '+') 
      { 
	add_char_to_text( token_text, next_char );
        read_next_char(scanner_input);
      }
      if (next_char < '0' || next_char > '9') 
	complain( "Missing exponent." );
      while (next_char >= '0' && next_char <= '9') 
      { 
	add_char_to_text( token_text, next_char );
        read_next_char(scanner_input);
      }  
    }
  }
  if (sscanf( token_text->buffer, "%lf", &token_number ) != 1) 
    complain( "Illegal number." );
}

/*---------------------------------------------------------------------------*/

static void 
read_string(string_t & scanner_input)
/* Read a string. Save its value in TOKEN_STRING. */
{
  int_t i;
  u_int_t code;

  token_name = NULL;
  clear_text( token_text );
  read_next_char(scanner_input); /* Overread beginning '"'. */
  while (next_char != '\"') 
  { 
    if (next_char == EOF || next_char == '\n') 
      complain( "Unterminated string at end of line." );
    if (next_char != '\\') 
    {
      add_unichar_to_text( token_text, next_char );
      read_next_char(scanner_input);
    }
    else
    { 
      read_next_char(scanner_input);
      if (next_char == '\\' || next_char == '\"')
      {
	add_char_to_text( token_text, next_char );
	read_next_char(scanner_input);
      }
      else if (next_char >= '0'  && next_char <= '7')
      { 
	code = 0;
	for (i = 0; i < 3; i++) 
	{ 
	  if (next_char >= '0' && next_char <= '7')
	    code = 8 * code + (next_char - '0');
	  else 
	    complain( "Escape sequence must have 3 octal digits." );
	  read_next_char(scanner_input);
	}
	if (! g_unichar_validate( code ))
	  complain( "Escape sequence defines invalid character." );
	add_unichar_to_text( token_text, code );
      }
      else 
	complain( "Illegal escape sequence in string." );
    }
  }
  read_next_char(scanner_input); /* Read over final '"'. */
  free_mem( &token_string ); /* Free old token string. */
  token_string = new_string( token_text->buffer, NULL );
}

/*---------------------------------------------------------------------------*/

void 
read_next_token(string_t & scanner_input)
/* Read the next token from current source into NEXT_TOKEN.
 * If end of input stream is reached, return EOF. */
{
  /* Read chars until a token has been recognised. */
  while (true) 
  { 
    switch (next_char) 
    {
    case EOF:
      next_token = EOF;
      return;
    case ' ': 
    case '\t': 
    case '\n': /* Read over whitespace. */
      read_next_char(scanner_input);
      break;
    case '\r':
      read_next_char(scanner_input);
      if (next_char != '\n')
	complain( "Carriage return without line feed." );
      read_next_char(scanner_input);
      break;
    case '#': /* Read over a comment. */
      do 
      { 
	read_next_char(scanner_input); 
      } while (next_char != '\n' && next_char != EOF);
      break;
    case '\"': /* Read a string. */
      read_string(scanner_input);
      next_token = TOK_STRING;
      return;
    case ':': /* Read a ":", ":=", ":=+", ":=-", ":=*", ":=/". */
      read_next_char(scanner_input);
      if (next_char == '=') 
      { 
	read_next_char(scanner_input);
        if (next_char == '+') 
	{ 
	  next_token = TOK_ASSIGN_PLUS;
	  read_next_char(scanner_input);
        } 
	else if (next_char == '-') 
	{ 
	  next_token = TOK_ASSIGN_MINUS;
          read_next_char(scanner_input);
        }
	else if (next_char == '*') 
	{ 
	  next_token = TOK_ASSIGN_ASTERISK;
          read_next_char(scanner_input);
        } 
	else if (next_char == '/') 
	{ 
	  next_token = TOK_ASSIGN_SLASH;
          read_next_char(scanner_input);
        } 
	else 
	  next_token = TOK_ASSIGN;
      } 
      else 
	next_token = ':';
      return;
    case '/': /* Read a "/", a "/=" or a "/~". */
      read_next_char(scanner_input);
      if (next_char == '=') 
      { 
	next_token = TOK_NOT_EQUAL;
        read_next_char(scanner_input);
      } 
      else if (next_char == '~') 
      { 
        next_token = TOK_NOT_CONGRUENT;
        read_next_char(scanner_input);
      } 
      else 
	next_token = '/';
      return;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9': 
      /* Read a number. */
      read_number(scanner_input);
      next_token = TOK_NUMBER;
      return;
    case '$':
      read_next_char(scanner_input);
      read_name(scanner_input);
      next_token = TOK_VARIABLE;
      return;
    case '@':
      read_next_char(scanner_input);
      read_name(scanner_input);
      next_token = TOK_CONSTANT;
      return;
    default: 
    if (g_unichar_isalpha( next_char ) 
        || next_char == '_' || next_char == '&' || next_char == '|') 
      { 
	read_name(scanner_input);
        next_token = keyword_code( token_name );
        return;
      } 
      else 
      { 
	next_token = next_char;
        read_next_char(scanner_input);
        return;
      }
    }
  }
}

/*---------------------------------------------------------------------------*/

string_t 
token_as_text( int_t token )
/* Return TOKEN as a string readable for humans.
 * The string must be freed after use. */
{
  int_t i;
  char token_buffer[2];

  /* Look if TOKEN is a keyword. */
  for (i = 0; i < NUMBER_OF_KEYWORDS; i++) 
  { 
    if (keywords[i].code == token) 
      return concat_strings( "\"", keywords[i].name, "\"", NULL );
  }
  
  switch (token) 
  {
  case EOF: 
    return new_string( "end of input", NULL ); 
  case TOK_STRING: 
    return new_string( "string", NULL );
  case TOK_IDENT: 
    return new_string( "identifier", NULL );
  case TOK_VARIABLE: 
    return new_string( "variable", NULL );
  case TOK_CONSTANT: 
    return new_string( "constant", NULL );
  case TOK_NUMBER: 
    return new_string( "number", NULL );
  case TOK_ASSIGN: 
    return new_string_readable( ":=", NULL );
  case TOK_ASSIGN_PLUS: 
    return new_string_readable( ":=+", NULL );
  case TOK_ASSIGN_MINUS: 
    return new_string_readable( ":=-", NULL );
  case TOK_ASSIGN_ASTERISK: 
    return new_string_readable( ":=*", NULL );
  case TOK_ASSIGN_SLASH: 
    return new_string_readable( ":=/", NULL );
  case TOK_NOT_EQUAL: 
    return new_string_readable( "/=", NULL );
  case TOK_NOT_CONGRUENT: 
    return new_string_readable( "/~", NULL );
  default:
    token_buffer[0] = token;
    token_buffer[1] = EOS;
    return new_string_readable( token_buffer, NULL );
  }
}

/*---------------------------------------------------------------------------*/

void 
test_token( int_t token )
/* Test if TOKEN is the next token. If it's not, report an error. */
{
  if (next_token != token) 
  { 
    complain( "Expected %s, not %s.", 
	      token_as_text( token ), token_as_text( next_token ) );
  }
}

/*---------------------------------------------------------------------------*/

void 
parse_token(int_t token, string_t & scanner_input)
/* Test if TOKEN is the next token and read next token. */
{
  test_token( token );
  read_next_token(scanner_input);
}

}}}
