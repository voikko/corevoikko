/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* Tools for a command line interpreter. */

/* Types. ===================================================================*/

/* Use a vector of "command_t" command descriptions
 * (terminated by a {NULL, NULL} entry) as argument for "command_loop". */ 
typedef struct
{ 
  string_t names; /* The command name followed by its
                   * shortcuts, separated by spaces. */
  void (*command)( string_t arguments ); /* The command function. */
  string_t help; /* Help string or NULL. */
} command_t; 

/* Variables. ===============================================================*/

extern command_t **options;
/* The options that can be set by the set command. */

extern bool leave_program; /* Set to TRUE if program is to quit. */
extern bool leave_command_loop; /* Set to TRUE if command loop is to quit. */

extern command_t help_command; /* Print a command summary. */

extern command_t quit_command; /* Quit the program. */

extern command_t get_command; /* Query options. */

extern command_t alias_option; /* Define command line abbreviations. */

/* End of file. =============================================================*/
