/* Copyright (C) 1997 Bjoern Beutel. */

/* Description. =============================================================*/

/* Options for malaga and functions to start and terminate malaga. */

namespace libvoikko { namespace morphology { namespace malaga {

class MalagaState;

/* Functions. ===============================================================*/

extern void init_malaga(string_t directoryName, MalagaState * malagaState);
/* Initialise this module. */

extern void terminate_malaga(MalagaState * malagaState);
/* Terminate this module. */

}}}
