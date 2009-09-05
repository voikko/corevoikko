/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module supports reading and parsing input. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

extern void parse_whitespace( string_t *input ); 
/* Read whitespace in *INPUT and update *INPUT. */

extern char_t *parse_word( string_t *input );
/* If there is a word in *INPUT, parse it up to the next space
 * and update *INPUT. Return the word. It must be freed after use.
 * If there is no word, report an error. */

extern char_t *parse_absolute_path( string_t *input, string_t relative_to );
/* Parse the next file name in *INPUT and update *INPUT.
 * Make the file name absolute (relative to RELATIVE_TO).
 * If there is no file name, report an error. */

extern char_t *read_line( FILE *stream );
/* Read user input from STREAM until eof or newline is met.
 * Return the result string (without final EOL or EOF). 
 * The string must be freed after use.
 * If EOF is initially met, return NULL. */

extern void cut_comment( char_t *line );
/* Cut a "#"-comment in LINE if there is one. 
 * The char "#" is ignored if it occurs within a double-quoted string. */

extern void init_input( void );
/* Initialise this module. */

extern void terminate_input( void );
/* Terminate this module. */

}}}
