/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module supports scanning (lexical analysis) of malaga source files. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants. ===============================================================*/

/* The next token is read by "read_next_token".
 * Its value is stored in NEXT_TOKEN.
 * A token that consists of one character has the code of that character.
 * A token value of EOF stands for end-of-file.
 * All other tokens are as follows: */
enum 
{ 
  TOK_STRING = 256, /* A string. */
  TOK_IDENT, /* An identifier. */
  TOK_VARIABLE, /* A variable name. */
  TOK_CONSTANT, /* A constant name. */
  TOK_NUMBER, /* A floating number. */ 
  TOK_ASSIGN, /* ":=". */
  TOK_ASSIGN_PLUS, /* ":=+". */
  TOK_ASSIGN_MINUS, /* ":=-". */
  TOK_ASSIGN_ASTERISK, /* ":=*". */
  TOK_ASSIGN_SLASH, /* ":=/". */
  TOK_NOT_EQUAL, /* "/=". */
  TOK_NOT_CONGRUENT, /* "/~". */
  TOK_ACCEPT,
  TOK_ALLO_RULE,
  TOK_AND,
  TOK_ASSERT,
  TOK_BREAK,
  TOK_CHOOSE,
  TOK_COMBI_RULE,
  TOK_CONTINUE,
  TOK_DEFAULT,
  TOK_DEFINE,
  TOK_ELSE,
  TOK_ELSEIF,
  TOK_END,
  TOK_END_RULE,
  TOK_ERROR,
  TOK_FOREACH,
  TOK_GREATER,
  TOK_GREATER_EQUAL,
  TOK_IF,
  TOK_IN,
  TOK_INCLUDE,
  TOK_INITIAL,
  TOK_INPUT_FILTER,
  TOK_LESS,
  TOK_LESS_EQUAL,
  TOK_MATCHES,
  TOK_NOT,
  TOK_OR,
  TOK_OUTPUT_FILTER,
  TOK_PARALLEL,
  TOK_PRUNING_RULE,
  TOK_REPEAT,
  TOK_REQUIRE,
  TOK_RESULT,
  TOK_RETURN,
  TOK_ROBUST_RULE,
  TOK_RULES,
  TOK_SELECT,
  TOK_STOP,
  TOK_SUBRULE,
  TOK_THEN,
  TOK_WHILE
};

enum {FIRST_KEYWORD = TOK_ACCEPT, LAST_KEYWORD = TOK_WHILE};

enum {NUMBER_OF_KEYWORDS = (LAST_KEYWORD - FIRST_KEYWORD + 1)};

/* Variables. ===============================================================*/

extern int_t next_token; /* Next token that is to be consumed by parser. FIXME */

extern string_t token_name; // FIXME
/* If NEXT_TOKEN == TOK_IDENT, the name is in TOKEN_NAME. */

extern char_t *token_string; // FIXME
/* If NEXT_TOKEN == TOK_STRING, the name is in TOKEN_STRING. */

extern double token_number; // FIXME
/* If NEXT_TOKEN == TOK_NUMBER, its content is in TOKEN_NUMBER. */

/* Functions. ===============================================================*/

extern void init_scanner( void );
/* Initialise the scanner. */

extern void terminate_scanner( void );
/* Terminate the scanner, even when it's scanning. */

extern void set_scanner_input( string_t input );
/* Make the scanner use INPUT as scanner input 
 * until "set_scanner_input( NULL )" is called.
 * INPUT must remain valid until then. */

extern void read_next_token( void );
/* Read the next token from current source into NEXT_TOKEN.
 * If end of input stream is reached, return EOF. */

extern void test_token( int_t token );
/* Test if TOKEN is the next token. If it's not, report an error. */

extern void parse_token( int_t token );
/* Test if TOKEN is the next token and read next token. */

extern string_t token_as_text( int_t token );
/* Return TOKEN as a string readable for humans.
 * Note that the string is only valid until this function is called again. */

}}}
