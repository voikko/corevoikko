/* Copyright (C) 1995 Bjoern Beutel. */

/* Description. =============================================================*/

/* This module defines the data types needed for Malaga rules. */

#ifndef LIBVOIKKO_MORPHOLOGY_MALAGA_RULE_TYPE_HPP
#define LIBVOIKKO_MORPHOLOGY_MALAGA_RULE_TYPE_HPP

namespace libvoikko { namespace morphology { namespace malaga {

/* Constants ================================================================*/

/* The rule-internal state is described by the following variables:
 * S is the value stack.
 * SP (stack pointer) is the index of the first free element in S.
 * BP (base pointer) is the index of the first element in S local to the
 *    current rule.
 * I is information that is stored in the instruction itself.
 * PC is the program counter, i.e., the index of the next code to be executed.
 * BT is the backtrace stack, a stack of triples: (PC, BP, S).
 * BTP is the top index of B. */
enum /* These are the opcodes of the rule instructions. */
{ 
  /* control instructions */
  INS_SYSTEM_ERROR, /* report error(I); terminate all paths. */
  INS_ERROR, /* report error( S[SP - 1] ); terminate all paths. */
  INS_TERMINATE, /* terminate this path. */
  INS_NOP, /* do nothing. */
  INS_TERMINATE_IF_NULL, /* if (S[SP - 1] == NULL) {terminate;} else {SP--;} */

  /* result instructions */
  INS_ADD_END_STATE, /* result = S[SP - 1]; rule_successful = true; SP--; */
  INS_ADD_STATE, /* result = S[SP - 1]; rules = rules[I];
                  * rule_successful = true; SP--; */
  INS_ADD_ALLO, /* surf = S[SP-2]; allo = S[SP - 1];
                 * rule_successful = true; SP-= 2 */
  INS_ACCEPT, /* rule_successful = true; */

  /* stack management instructions */
  INS_PUSH_NULL, /* for (i = 0; i < I; i++) { S[SP + i] = NULL; } SP += I; */
  INS_PUSH_VAR, /* S[SP] = S[I]; SP++; */
  INS_PUSH_CONST, /* S[SP] = Const(I); SP++; */
  INS_PUSH_SYMBOL, /* S[SP] = Symbol(I); SP++; */
  INS_PUSH_PATTERN_VAR, /* S[SP] = Pattern_Var(I); SP++; */
  INS_POP, /* SP -= I; */
  INS_POP_TO, /* SP = BP + I; */

  /* value instructions */
  INS_BUILD_LIST, /* S[SP - I] = "<"S[SP - I],.., S[SP - 1]">"; SP -= I - 1; */
  INS_DECOMPOSE_LIST, /* <S[SP - 1], .. , S[SP + I - 2]> = S[SP - 1]; 
		       * SP += I - 1; */
  INS_BUILD_RECORD, /* S[SP - 2 * I] = "[" S[SP - 2 * I] : S[SP - 2 * I + 1] 
		     *                     ... 
		     *                     S[SP - 2] : S[SP - 1] "]"; 
		     * SP -= 2*I - 1; */
  INS_BUILD_PATH, /* SP[SP - I] = S[SP - I] "." .. "." S[SP - 1]; SP -= I-1; */
  INS_DOT_OPERATION, /* S[SP - 2] = S[SP - 2] "." S[SP - 1]; SP--; */
  INS_PLUS_OPERATION, /* S[SP - 2] := S[SP - 2] "+" S[SP - 1]; SP--; */
  INS_MINUS_OPERATION, /* S[SP - 2] = S[SP - 2] "-" S[SP - 1]; SP--; */
  INS_ASTERISK_OPERATION, /* S[SP - 2] = S[SP - 2] "*" S[SP - 1]; SP--; */
  INS_SLASH_OPERATION, /* S[SP - 2] = S[SP - 2] "/" S[SP - 1]; SP--; */
  INS_UNARY_MINUS_OP, /* S[SP - 1] = "-" S[SP - 1]; */
  INS_GET_ATTRIBUTE, /* S[SP - 1] = S[SP - 1] "." Symbol(I); */
  INS_REMOVE_ATTRIBUTE, /* S[SP - 1] = S[SP - 1] "-" Symbol(I); */
  INS_STD_FUNCTION, /* S[SP - 1] = function_I( S[SP - 1] ); */
  INS_MATCH, /* S[SP - 1] = S[SP - 1] "match" String(I); (yes or no) */

  /* instructions to modify variables */
  INS_SET_VAR, /* S[I] = S[SP - 1]; SP--; */
  INS_PLUS_VAR, /* S[I] = S[I] "+" S[SP - 1]; SP--; */
  INS_MINUS_VAR, /* S[I] = S[I] "-" S[SP - 1]; SP--; */
  INS_ASTERISK_VAR, /* S[I] = S[I] "*" S[SP - 1]; SP--; */
  INS_SLASH_VAR, /* S[I] = S[I] "/" S[SP - 1]; SP--; */
  INS_SET_VAR_PATH, /* (S[I] "." S[SP - 2]) = S[SP - 1]; SP -= 2; */
  INS_PLUS_VAR_PATH, /* (S[I] "." S[SP-2]) = (S[I] "." S[SP-2]) "+" S[SP-1]; 
		      * SP -= 2; */
  INS_MINUS_VAR_PATH, /* (S[I] "." S[SP-2]) = (S[I] "." S[SP-2]) "-" S[SP-1]; 
		       * SP -= 2; */
  INS_ASTERISK_VAR_PATH, /* (S[I]"." S[SP-2]) = (S[I] "." S[SP-2]) "*" S[SP-1];
			  * SP -= 2; */
  INS_SLASH_VAR_PATH, /* (S[I] "." S[SP-2]) = (S[I] "." S[SP-2]) "/" S[SP-1]; 
		       * SP -= 2; */

  /* Instructions to support loops. */
  INS_GET_1ST_ELEMENT, /* S[SP-1] = 1st element/attrib/ordinal of S[SP-1]; */
  INS_ITERATE, /* S[I - 1] must be a list, a record, or a number
                * and S[I] the n-th element/attrib/ordinal of S[I - 1].
                * S[I] = (n+1)-th element/attr/ordinal of S[I - 1]; */

  /* Jump instructions. */
  INS_JUMP, /* PC = I. */
  INS_JUMP_IF_EQUAL, /* if (S[SP-2] "=" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NOT_EQUAL, /* if (! S[SP-2] "=" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_CONGR, /* if (S[SP-2] "~" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NOT_CONGR, /* if (! S[SP-2] "~" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_IN, /* if (S[SP-2] "in" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NOT_IN, /* if (! S[SP-2] "in" S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_LESS, /* if (S[SP-2] < S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NOT_LESS, /* if (! S[SP-2] < S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_GREATER, /* if (S[SP-2] > S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NOT_GREATER, /* if (! S[SP-2] > S[SP-1]) {PC = I;} SP -= 2; */
  INS_JUMP_IF_NULL, /* if (S[SP-1] == NULL) {PC = I;} SP--; */
  INS_JUMP_IF_NOT_NULL, /* if (! S[SP-1] == NULL) {PC = I;} SP--; */
  INS_JUMP_IF_YES, /* if (S[SP-1] == yes) {PC = I;} SP--; */
  INS_JUMP_IF_NO, /* if (S[SP-1] == no) {PC = I;} SP--; */
  INS_JUMP_NOW, /* BT[BTP] = {PC, BP, S}; PC = I; BTP++; */
  INS_JUMP_LATER, /* BT[BTP] = {I, BP, S}; BTP++; */
  INS_JUMP_SUBRULE, /* Push BP; Push PC+1; BP = TOP; PC = first_instr(I); */
  INS_RETURN /* SP = BP; Pop PC; Pop BP; Pop (I-1); */
};

/* Types of INS_SYSTEM_ERROR. */
enum {ASSERTION_ERROR, NO_RETURN_ERROR};

/* Standard functions for INS_STANDARD_FUNCTION. */
enum {FUNC_TO_ATOMS, FUNC_IS_CAPITAL, FUNC_GET_LENGTH, FUNC_TO_MULTI, 
      FUNC_TO_SET, FUNC_GET_SWITCH, FUNC_GET_VALUE_TYPE, FUNC_GET_VALUE_STRING,
      FUNC_TRANSMIT, FUNC_FLOOR, FUNC_SUBSTRING};

#define INSTR_INFO_MIN (-1L << 23)
#define INSTR_INFO_MAX ((1L << 23) - 1)
/* Never use an instruction info smaller than INSTR_INFO_MIN or
 * greater than INSTR_INFO_MAX. */

enum {OPCODE_MAX = 255}; /* The largest opcode possible. */

/* Macros. ==================================================================*/

#define INSTR(opcode, instr_info) ((opcode) | (u_int_t) (instr_info) << 8)
/* Use this macro to create an instruction. */ 

#define OPCODE(instr) ((int_t) (instr) & OPCODE_MAX)
/* Use this macro to get the opcode of an instruction. */

#define INSTR_INFO(instr) ((int_t) (instr) >> 8)
/* Use this macro to get the info of an instruction. */

/* Types. ===================================================================*/

typedef u_int_t instr_t;
/* An instruction is an u_int_t whose lower 8 bits store the opcode. 
 * The upper 24 bits store a signed value, the Info value. */

typedef enum /* Rule types */
{ALLO_RULE, COMBI_RULE, END_RULE, FILTER_RULE, PRUNING_RULE, ROBUST_RULE, 
 SUBRULE} rule_type_t;

typedef struct /* A rule definition.*/
{ 
  int_t name; /* String table index to rule name. */
  int_t first_instr; /* Code index of rule start. */
  rule_type_t type; /* Type of the rule. */
  int_t param_count; /* Number of parameters the rule takes. */
} rule_t;

typedef struct
/* A correspondence between a source line and a rule code index. */
{ 
  int_t line; /* Source line or -1 if instruction can't be
               * associated with a specific source line */
  int_t file; /* String table index to source file name or -1. */
  int_t instr; /* Index of first instruction that belongs to LINE in FILE. */
} src_line_t;

typedef struct /* A variable name and all its scopes. */
{ 
  int_t name; /* String table index of variable name. */
  int_t first_scope; /* Index of first scope of this variable in VAR_SCOPES. */
  int_t scope_count; /* Number of scopes of this variable. */ 
} var_t;

typedef struct /* A single scope of a variable. */
{ 
  int_t first_instr; /* First instruction where variable is defined. */
  int_t last_instr; /* Last instruction where variable is defined. */
  int_t stack_index; /* Index of variable in value stack. */
} var_scope_t;

typedef struct /* A named constant. */
{
  int_t name; /* String table index of constant name. */
  int_t value; /* Value index. */
} constant_t;

}}}

#endif
