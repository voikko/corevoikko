/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* Options for malaga and functions to start and terminate malaga. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

extern bool auto_tree; /* true if tree is shown automatically. */
extern bool auto_result; /* true if result is shown automatically. */
extern bool result_as_list; /* true if results will be combined into 
			       * a list. */
extern text_t *grammar_info; /* Information about grammar. */

extern string_t result_format, unknown_format, error_format; 
/* Format strings for output. */

/* Functions. ===============================================================*/

extern void init_malaga( string_t project_file );
/* Initialise this module. */

extern void terminate_malaga( void );
/* Terminate this module. */

}}}
