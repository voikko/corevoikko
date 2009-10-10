/* Copyright (C) 2002 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module parses Malaga values. */

namespace libvoikko { namespace morphology { namespace malaga {

/* Functions. ===============================================================*/

extern void parse_a_value(string_t & scanner_input);
/* Stack effects: (nothing) -> NEW_VALUE.
 * Parse a value (use scanner input) and leave it on the stack. */

}}}
