/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* Options for malaga and functions to start and terminate malaga. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Variables. ===============================================================*/

extern text_t *grammar_info; /* Information about grammar. FIXME */

/* Functions. ===============================================================*/

extern void init_malaga( string_t project_file );
/* Initialise this module. */

extern void terminate_malaga( void );
/* Terminate this module. */

}}}
