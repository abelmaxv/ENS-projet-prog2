
module MenhirBasics = struct
  
  exception Error
  
  let _eRR =
    fun _s ->
      raise Error
  
  type token = 
    | WHILE
    | TILDE_CHR
    | SUB_CHR
    | STRING_LITERAL of (
# 45 "cparser.mly"
       (string)
# 18 "cparser.ml"
  )
    | STAR_CHR
    | SEMI_CHR
    | RETURN
    | QUES_CHR
    | OR_OP
    | OPEN_PAREN_CHR
    | OPEN_BRACE_CHR
    | OPEN_ANGLE_CHR
    | NE_OP
    | MOD_CHR
    | LE_OP
    | INTEGER
    | INC_OP
    | IF
    | IDENTIFIER of (
# 43 "cparser.mly"
       (string)
# 37 "cparser.ml"
  )
    | GE_OP
    | FOR
    | EQ_OP
    | EQ_CHR
    | EOF
    | ELSE
    | DIV_CHR
    | DEC_OP
    | CONSTANT of (
# 44 "cparser.mly"
       (int)
# 50 "cparser.ml"
  )
    | COMMA_CHR
    | COLON_CHR
    | CLOSE_PAREN_CHR
    | CLOSE_BRACE_CHR
    | CLOSE_ANGLE_CHR
    | BANG_CHR
    | AND_OP
    | AMP_CHR
    | ADD_CHR
  
end

include MenhirBasics

# 1 "cparser.mly"
  

(*
 *	Copyright (C) 2023 by Laboratoire Méthodes Formelles (LMF),
 *      UMR 9021  Université Paris-Saclay, CNRS et ENS Paris-Saclay.
 *      Modified by Mihaela Sighireanu.
 *
 *      Copyright (C) 2005, 2006 by Laboratoire Spécification et Vérification (LSV),
 *      UMR 8643 CNRS & ENS Cachan.
 *      Written by Jean Goubault-Larrecq.  Derived from the csur project.
 *
 *      Permission is granted to anyone to use this software for any
 *      purpose on any computer system, and to redistribute it freely,
 *      subject to the following restrictions:
 *
 *      1. Neither the author nor its employer is responsible for the consequences of use of
 *              this software, no matter how awful, even if they arise
 *              from defects in it.
 *
 *      2. The origin of this software must not be misrepresented, either
 *              by explicit claim or by omission.
 *
 *      3. Altered versions must be plainly marked as such, and must not
 *              be misrepresented as being the original software.
 *
 *      4. This software is restricted to non-commercial use only.  Commercial
 *              use is subject to a specific license, obtainable from LMF.
 *
*)

(* Analyse syntaxique d'un sous-ensemble (tres) reduit de C.
 *)

open Cast

exception Parse_error of Cast.location * string
let sup_locator loc1 loc2 = 
  let st1, end1 = loc1 in
  let st2, end2 = loc2 in st1, end2


# 108 "cparser.ml"

type ('s, 'r) _menhir_state = 
  | MenhirState000 : ('s, _menhir_box_file) _menhir_state
    (** State 000.
        Stack shape : .
        Start symbol: file. *)

  | MenhirState003 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 003.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState007 : ((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 007.
        Stack shape : type_specifier identifier.
        Start symbol: file. *)

  | MenhirState009 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 009.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState015 : (((('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list, _menhir_box_file) _menhir_state
    (** State 015.
        Stack shape : type_specifier identifier parameter_list.
        Start symbol: file. *)

  | MenhirState022 : (('s, _menhir_box_file) _menhir_cell1_function_declarator, _menhir_box_file) _menhir_state
    (** State 022.
        Stack shape : function_declarator.
        Start symbol: file. *)

  | MenhirState025 : (('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_state
    (** State 025.
        Stack shape : open_block.
        Start symbol: file. *)

  | MenhirState029 : (('s, _menhir_box_file) _menhir_cell1_STRING_LITERAL, _menhir_box_file) _menhir_state
    (** State 029.
        Stack shape : STRING_LITERAL.
        Start symbol: file. *)

  | MenhirState034 : (('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 034.
        Stack shape : OPEN_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState041 : (('s, _menhir_box_file) _menhir_cell1_unary_operator, _menhir_box_file) _menhir_state
    (** State 041.
        Stack shape : unary_operator.
        Start symbol: file. *)

  | MenhirState048 : (('s, _menhir_box_file) _menhir_cell1_postfix_expression, _menhir_box_file) _menhir_state
    (** State 048.
        Stack shape : postfix_expression.
        Start symbol: file. *)

  | MenhirState051 : (('s, _menhir_box_file) _menhir_cell1_inc_op, _menhir_box_file) _menhir_state
    (** State 051.
        Stack shape : inc_op.
        Start symbol: file. *)

  | MenhirState054 : (('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_state
    (** State 054.
        Stack shape : identifier.
        Start symbol: file. *)

  | MenhirState057 : (('s, _menhir_box_file) _menhir_cell1_unary_expression, _menhir_box_file) _menhir_state
    (** State 057.
        Stack shape : unary_expression.
        Start symbol: file. *)

  | MenhirState060 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 060.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState063 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 063.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState064 : (('s, _menhir_box_file) _menhir_cell1_dec_op, _menhir_box_file) _menhir_state
    (** State 064.
        Stack shape : dec_op.
        Start symbol: file. *)

  | MenhirState071 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 071.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState073 : (('s, _menhir_box_file) _menhir_cell1_multiplicative_expression, _menhir_box_file) _menhir_state
    (** State 073.
        Stack shape : multiplicative_expression.
        Start symbol: file. *)

  | MenhirState077 : (('s, _menhir_box_file) _menhir_cell1_additive_expression, _menhir_box_file) _menhir_state
    (** State 077.
        Stack shape : additive_expression.
        Start symbol: file. *)

  | MenhirState079 : (('s, _menhir_box_file) _menhir_cell1_additive_expression, _menhir_box_file) _menhir_state
    (** State 079.
        Stack shape : additive_expression.
        Start symbol: file. *)

  | MenhirState081 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 081.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState083 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 083.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState085 : (('s, _menhir_box_file) _menhir_cell1_relational_expression, _menhir_box_file) _menhir_state
    (** State 085.
        Stack shape : relational_expression.
        Start symbol: file. *)

  | MenhirState088 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 088.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState090 : (('s, _menhir_box_file) _menhir_cell1_logical_and_expression, _menhir_box_file) _menhir_state
    (** State 090.
        Stack shape : logical_and_expression.
        Start symbol: file. *)

  | MenhirState094 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 094.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState096 : (('s, _menhir_box_file) _menhir_cell1_equality_expression, _menhir_box_file) _menhir_state
    (** State 096.
        Stack shape : equality_expression.
        Start symbol: file. *)

  | MenhirState100 : ((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 100.
        Stack shape : logical_or_expression expression.
        Start symbol: file. *)

  | MenhirState101 : ((('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 101.
        Stack shape : expression COMMA_CHR.
        Start symbol: file. *)

  | MenhirState104 : (((('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR, _menhir_box_file) _menhir_state
    (** State 104.
        Stack shape : logical_or_expression expression COLON_CHR.
        Start symbol: file. *)

  | MenhirState107 : (('s, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_state
    (** State 107.
        Stack shape : logical_or_expression.
        Start symbol: file. *)

  | MenhirState112 : ((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_state
    (** State 112.
        Stack shape : identifier argument_expression_list.
        Start symbol: file. *)

  | MenhirState113 : (((('s, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR, _menhir_box_file) _menhir_state
    (** State 113.
        Stack shape : identifier argument_expression_list COMMA_CHR.
        Start symbol: file. *)

  | MenhirState117 : ((('s, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 117.
        Stack shape : OPEN_PAREN_CHR expression.
        Start symbol: file. *)

  | MenhirState123 : (('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_state
    (** State 123.
        Stack shape : whilekw.
        Start symbol: file. *)

  | MenhirState124 : ((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 124.
        Stack shape : whilekw expression.
        Start symbol: file. *)

  | MenhirState125 : (((('s, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 125.
        Stack shape : whilekw expression close_paren.
        Start symbol: file. *)

  | MenhirState129 : (('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_state
    (** State 129.
        Stack shape : return.
        Start symbol: file. *)

  | MenhirState131 : ((('s, _menhir_box_file) _menhir_cell1_return, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 131.
        Stack shape : return expression.
        Start symbol: file. *)

  | MenhirState136 : (('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_state
    (** State 136.
        Stack shape : ifkw.
        Start symbol: file. *)

  | MenhirState137 : ((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 137.
        Stack shape : ifkw expression.
        Start symbol: file. *)

  | MenhirState138 : (((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_state
    (** State 138.
        Stack shape : ifkw expression CLOSE_PAREN_CHR.
        Start symbol: file. *)

  | MenhirState141 : (('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_state
    (** State 141.
        Stack shape : forkw.
        Start symbol: file. *)

  | MenhirState142 : ((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 142.
        Stack shape : forkw expression_statement.
        Start symbol: file. *)

  | MenhirState143 : (((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_state
    (** State 143.
        Stack shape : forkw expression_statement expression_statement.
        Start symbol: file. *)

  | MenhirState144 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 144.
        Stack shape : forkw expression_statement expression_statement expression.
        Start symbol: file. *)

  | MenhirState145 : (((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 145.
        Stack shape : forkw expression_statement expression_statement expression close_paren.
        Start symbol: file. *)

  | MenhirState148 : (('s, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_state
    (** State 148.
        Stack shape : expression.
        Start symbol: file. *)

  | MenhirState151 : ((((('s, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren, _menhir_box_file) _menhir_state
    (** State 151.
        Stack shape : forkw expression_statement expression_statement close_paren.
        Start symbol: file. *)

  | MenhirState154 : ((((('s, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement, _menhir_box_file) _menhir_state
    (** State 154.
        Stack shape : ifkw expression CLOSE_PAREN_CHR compound_statement.
        Start symbol: file. *)

  | MenhirState157 : (('s, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_state
    (** State 157.
        Stack shape : type_specifier.
        Start symbol: file. *)

  | MenhirState158 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 158.
        Stack shape : open_block statement_list.
        Start symbol: file. *)

  | MenhirState163 : ((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_state
    (** State 163.
        Stack shape : open_block declaration_list.
        Start symbol: file. *)

  | MenhirState164 : (((('s, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list, _menhir_box_file) _menhir_state
    (** State 164.
        Stack shape : open_block declaration_list statement_list.
        Start symbol: file. *)

  | MenhirState172 : (('s, _menhir_box_file) _menhir_cell1_external_declaration, _menhir_box_file) _menhir_state
    (** State 172.
        Stack shape : external_declaration.
        Start symbol: file. *)


and ('s, 'r) _menhir_cell1_additive_expression = 
  | MenhirCell1_additive_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_argument_expression_list = 
  | MenhirCell1_argument_expression_list of 's * ('s, 'r) _menhir_state * (Cast.loc_expr list)

and ('s, 'r) _menhir_cell1_close_paren = 
  | MenhirCell1_close_paren of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_compound_statement = 
  | MenhirCell1_compound_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_code)

and ('s, 'r) _menhir_cell1_dec_op = 
  | MenhirCell1_dec_op of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_declaration_list = 
  | MenhirCell1_declaration_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_equality_expression = 
  | MenhirCell1_equality_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression = 
  | MenhirCell1_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_expression_statement = 
  | MenhirCell1_expression_statement of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_external_declaration = 
  | MenhirCell1_external_declaration of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_forkw = 
  | MenhirCell1_forkw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_function_declarator = 
  | MenhirCell1_function_declarator of 's * ('s, 'r) _menhir_state * ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp)

and ('s, 'r) _menhir_cell1_identifier = 
  | MenhirCell1_identifier of 's * ('s, 'r) _menhir_state * (Cast.location * string)

and ('s, 'r) _menhir_cell1_ifkw = 
  | MenhirCell1_ifkw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_inc_op = 
  | MenhirCell1_inc_op of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_logical_and_expression = 
  | MenhirCell1_logical_and_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_logical_or_expression = 
  | MenhirCell1_logical_or_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_multiplicative_expression = 
  | MenhirCell1_multiplicative_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_open_block = 
  | MenhirCell1_open_block of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_parameter_list = 
  | MenhirCell1_parameter_list of 's * ('s, 'r) _menhir_state * (Cast.var_declaration list)

and ('s, 'r) _menhir_cell1_postfix_expression = 
  | MenhirCell1_postfix_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_relational_expression = 
  | MenhirCell1_relational_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_return = 
  | MenhirCell1_return of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_statement_list = 
  | MenhirCell1_statement_list of 's * ('s, 'r) _menhir_state * (Cast.loc_code list)

and ('s, 'r) _menhir_cell1_type_specifier = 
  | MenhirCell1_type_specifier of 's * ('s, 'r) _menhir_state * (Cast.ctyp)

and ('s, 'r) _menhir_cell1_unary_expression = 
  | MenhirCell1_unary_expression of 's * ('s, 'r) _menhir_state * (Cast.loc_expr)

and ('s, 'r) _menhir_cell1_unary_operator = 
  | MenhirCell1_unary_operator of 's * ('s, 'r) _menhir_state * (Cast.location * token)

and ('s, 'r) _menhir_cell1_whilekw = 
  | MenhirCell1_whilekw of 's * ('s, 'r) _menhir_state * (Cast.location)

and ('s, 'r) _menhir_cell1_CLOSE_PAREN_CHR = 
  | MenhirCell1_CLOSE_PAREN_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COLON_CHR = 
  | MenhirCell1_COLON_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_COMMA_CHR = 
  | MenhirCell1_COMMA_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_OPEN_PAREN_CHR = 
  | MenhirCell1_OPEN_PAREN_CHR of 's * ('s, 'r) _menhir_state

and ('s, 'r) _menhir_cell1_STRING_LITERAL = 
  | MenhirCell1_STRING_LITERAL of 's * ('s, 'r) _menhir_state * (
# 45 "cparser.mly"
       (string)
# 492 "cparser.ml"
)

and _menhir_box_file = 
  | MenhirBox_file of (Cast.var_declaration list) [@@unboxed]

let _menhir_action_001 =
  fun () ->
    (
# 144 "cparser.mly"
                        ( getloc (), ADD_CHR   )
# 503 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_002 =
  fun _1 ->
    (
# 175 "cparser.mly"
            ( _1 )
# 511 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_003 =
  fun _1 _3 ->
    (
# 177 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_ADD, _1, _3)
	)
# 521 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_004 =
  fun _1 _3 ->
    (
# 181 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_SUB, _1, _3)
	)
# 531 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_005 =
  fun () ->
    (
# 149 "cparser.mly"
                        ( getloc (), AMP_CHR   )
# 539 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_006 =
  fun _1 ->
    (
# 226 "cparser.mly"
                              ( _1 )
# 547 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_007 =
  fun _1 ->
    (
# 109 "cparser.mly"
                                ( [_1] )
# 555 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_008 =
  fun _1 _3 ->
    (
# 110 "cparser.mly"
                                                                   ( 
          _3 :: _1 )
# 564 "cparser.ml"
     : (Cast.loc_expr list))

let _menhir_action_009 =
  fun _1 ->
    (
# 263 "cparser.mly"
                                 ( _1 )
# 572 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_010 =
  fun _1 _3 ->
    (
# 265 "cparser.mly"
     (
	     let locvar, left = _1 in
	     let loc = sup_locator locvar (loc_of_expr _3) in
	     match left with
	       VAR x -> loc, SET_VAR (x, _3)
	     | OP1 (M_DEREF, (_, VAR x)) -> loc, SET_VAL (x, _3)
	     | _ ->
		 raise (Parse_error (loc,
		     "Can only write assignments of the form x=e or *x=e.\n"))
	   )
# 589 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_011 =
  fun () ->
    (
# 146 "cparser.mly"
                        ( getloc (), BANG_CHR  )
# 597 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_012 =
  fun _1 ->
    (
# 154 "cparser.mly"
                           ( _1 )
# 605 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_013 =
  fun _1 ->
    (
# 313 "cparser.mly"
                          ( _1 )
# 613 "cparser.ml"
     : (Cast.location))

let _menhir_action_014 =
  fun () ->
    (
# 74 "cparser.mly"
                              ( getloc () )
# 621 "cparser.ml"
     : (Cast.location))

let _menhir_action_015 =
  fun () ->
    (
# 151 "cparser.mly"
                              ( getloc () )
# 629 "cparser.ml"
     : (Cast.location))

let _menhir_action_016 =
  fun _1 _2 ->
    (
# 317 "cparser.mly"
        ( sup_locator _1 _2, CBLOCK ([], []) )
# 637 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_017 =
  fun _1 _2 _3 ->
    (
# 319 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK ([], List.rev _2) )
# 645 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_018 =
  fun _1 _2 _3 ->
    (
# 321 "cparser.mly"
 ( sup_locator _1 _3, CBLOCK (_2, []) )
# 653 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_019 =
  fun _1 _2 _3 _4 ->
    (
# 323 "cparser.mly"
 ( sup_locator _1 _4, CBLOCK (_2, List.rev _3) )
# 661 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_020 =
  fun _1 ->
    (
# 254 "cparser.mly"
                                ( _1 )
# 669 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_021 =
  fun _1 _3 _5 ->
    (
# 256 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _5),
	  EIF (_1, _3, _5)
	)
# 680 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_022 =
  fun _1 ->
    (
# 70 "cparser.mly"
                    ( getloc (), _1 )
# 688 "cparser.ml"
     : (Cast.location * int))

let _menhir_action_023 =
  fun () ->
    (
# 86 "cparser.mly"
                ( getloc () )
# 696 "cparser.ml"
     : (Cast.location))

let _menhir_action_024 =
  fun _1 _2 ->
    (
# 288 "cparser.mly"
        ( let loc,var = _2 in [CDECL(loc,var,_1)] )
# 704 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_025 =
  fun _1 ->
    (
# 329 "cparser.mly"
          ( _1 )
# 712 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_026 =
  fun _1 _2 ->
    (
# 331 "cparser.mly"
          ( _1 @ _2 )
# 720 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_027 =
  fun _1 ->
    (
# 292 "cparser.mly"
                     ( _1 )
# 728 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_028 =
  fun _1 ->
    (
# 211 "cparser.mly"
                                ( _1 )
# 736 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_029 =
  fun _1 _3 ->
    (
# 213 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_EQ, _1, _3)
	)
# 746 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_030 =
  fun _1 _3 ->
    (
# 217 "cparser.mly"
 ( 
          let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF ((loc, CMP (C_EQ, _1, _3)),
		    (loc, CST 0),
		    (loc, CST 1))
	)
# 759 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_031 =
  fun _1 ->
    (
# 230 "cparser.mly"
                         ( _1 )
# 767 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_032 =
  fun _1 ->
    (
# 278 "cparser.mly"
                                ( _1 )
# 775 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_033 =
  fun _1 _3 ->
    (
# 280 "cparser.mly"
 ( 
	  sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  ESEQ [_1; _3]
	)
# 786 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_034 =
  fun _1 ->
    (
# 344 "cparser.mly"
            ( _1, ESEQ [] )
# 794 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_035 =
  fun _1 ->
    (
# 346 "cparser.mly"
            ( _1 )
# 802 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_036 =
  fun _1 ->
    (
# 414 "cparser.mly"
            ( [_1] )
# 810 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_037 =
  fun _1 ->
    (
# 416 "cparser.mly"
            ( _1 )
# 818 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_038 =
  fun () ->
    (
# 407 "cparser.mly"
          ( [] )
# 826 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_039 =
  fun _1 _2 ->
    (
# 409 "cparser.mly"
          ( _1 @ _2 )
# 834 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_040 =
  fun () ->
    (
# 370 "cparser.mly"
            ( getloc () )
# 842 "cparser.ml"
     : (Cast.location))

let _menhir_action_041 =
  fun _1 _2 _3 ->
    (
# 440 "cparser.mly"
 ( _2, _3, _1 )
# 850 "cparser.ml"
     : ((Cast.location * string) * Cast.var_declaration list * Cast.ctyp))

let _menhir_action_042 =
  fun _1 _2 ->
    (
# 445 "cparser.mly"
 ( 
          let (loc, var), decls, rty = _1 in
	  CFUN (loc, var, decls, rty, _2)
	)
# 861 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_043 =
  fun _1 ->
    (
# 72 "cparser.mly"
                              ( getloc (), _1 )
# 869 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_044 =
  fun () ->
    (
# 351 "cparser.mly"
          ( getloc () )
# 877 "cparser.ml"
     : (Cast.location))

let _menhir_action_045 =
  fun () ->
    (
# 85 "cparser.mly"
                ( getloc () )
# 885 "cparser.ml"
     : (Cast.location))

let _menhir_action_046 =
  fun _1 ->
    (
# 234 "cparser.mly"
                                  ( _1 )
# 893 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_047 =
  fun _1 _3 _5 ->
    (
# 373 "cparser.mly"
    (
	    let loc = sup_locator _1 (fst _5) in
	    loc, CWHILE (_3, _5)
	   )
# 904 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_048 =
  fun _1 _3 _4 _6 ->
    (
# 379 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _6) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4, _6)])
	)
# 916 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_049 =
  fun _1 _3 _4 _5 _7 ->
    (
# 386 "cparser.mly"
 ( 
          let loc = sup_locator _1 (fst _7) in
	  loc, CBLOCK ([], [(loc_of_expr _3, CEXPR _3);
			    loc, CWHILE (_4,
					 (sup_locator (loc_of_expr _5) (loc_of_expr _7),
					  CBLOCK ([], [_7; (loc_of_expr _5,
							    CEXPR _5)])))])
	)
# 931 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_050 =
  fun _1 ->
    (
# 400 "cparser.mly"
            ( _1, CRETURN None )
# 939 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_051 =
  fun _1 _2 ->
    (
# 402 "cparser.mly"
            ( sup_locator _1 (loc_of_expr _2), CRETURN (Some _2) )
# 947 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_052 =
  fun _1 ->
    (
# 238 "cparser.mly"
                                  ( _1 )
# 955 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_053 =
  fun _1 _3 ->
    (
# 240 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, _3, (loc, CST 0))
	)
# 965 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_054 =
  fun _1 ->
    (
# 246 "cparser.mly"
                                 ( _1 )
# 973 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_055 =
  fun _1 _3 ->
    (
# 248 "cparser.mly"
 ( let loc = sup_locator (loc_of_expr _1) (loc_of_expr _3) in
	  loc, EIF (_1, (loc, CST 1), _3)
	)
# 983 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_056 =
  fun _1 ->
    (
# 158 "cparser.mly"
                          ( _1 )
# 991 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_057 =
  fun _1 _3 ->
    (
# 160 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MUL, _1, _3)
	)
# 1001 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_058 =
  fun _1 _3 ->
    (
# 164 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_DIV, _1, _3)
	)
# 1011 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_059 =
  fun _1 _3 ->
    (
# 168 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  OP2 (S_MOD, _1, _3)
	)
# 1021 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_060 =
  fun _1 ->
    (
# 312 "cparser.mly"
                        ( _1 )
# 1029 "cparser.ml"
     : (Cast.location))

let _menhir_action_061 =
  fun () ->
    (
# 73 "cparser.mly"
                              ( getloc () )
# 1037 "cparser.ml"
     : (Cast.location))

let _menhir_action_062 =
  fun _1 _2 ->
    (
# 420 "cparser.mly"
          ( let _, vname = _2 in CDECL(getloc(), vname, _1) )
# 1045 "cparser.ml"
     : (Cast.var_declaration))

let _menhir_action_063 =
  fun () ->
    (
# 435 "cparser.mly"
                                  ( [] )
# 1053 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_064 =
  fun _2 ->
    (
# 436 "cparser.mly"
                                                      ( _2 )
# 1061 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_065 =
  fun _1 ->
    (
# 425 "cparser.mly"
          ( [_1] )
# 1069 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_066 =
  fun _1 _3 ->
    (
# 427 "cparser.mly"
          ( _3 :: _1 )
# 1077 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_067 =
  fun _1 ->
    (
# 431 "cparser.mly"
                         ( List.rev _1)
# 1085 "cparser.ml"
     : (Cast.var_declaration list))

let _menhir_action_068 =
  fun _1 ->
    (
# 89 "cparser.mly"
                             ( _1 )
# 1093 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_069 =
  fun _1 _3 ->
    (
# 91 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _3 in
	    loc1, CALL (var, [])
	)
# 1104 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_070 =
  fun _1 _3 _4 ->
    (
# 96 "cparser.mly"
 ( let loc, var = _1 in
	  let loc1 = sup_locator loc _4 in
	    loc1, CALL (var, List.rev _3)
	)
# 1115 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_071 =
  fun _1 _2 ->
    (
# 101 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_INC, _1) )
# 1123 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_072 =
  fun _1 _2 ->
    (
# 103 "cparser.mly"
 ( sup_locator (loc_of_expr _1) _2, OP1 (M_POST_DEC, _1) )
# 1131 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_073 =
  fun _1 ->
    (
# 64 "cparser.mly"
                     ( let loc, var = _1 in loc, VAR var )
# 1139 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_074 =
  fun _1 ->
    (
# 65 "cparser.mly"
                   ( let loc, cst = _1 in loc, CST cst )
# 1147 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_075 =
  fun _1 ->
    (
# 66 "cparser.mly"
                         ( let loc, s = _1 in loc, STRING s )
# 1155 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_076 =
  fun _2 ->
    (
# 67 "cparser.mly"
                                                    ( _2 )
# 1163 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_077 =
  fun _1 ->
    (
# 191 "cparser.mly"
                           ( _1 )
# 1171 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_078 =
  fun _1 _3 ->
    (
# 193 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _1, _3)
	)
# 1181 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_079 =
  fun _1 _3 ->
    (
# 197 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LT, _3, _1)
	)
# 1191 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_080 =
  fun _1 _3 ->
    (
# 201 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _1, _3)
	)
# 1201 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_081 =
  fun _1 _3 ->
    (
# 205 "cparser.mly"
 ( sup_locator (loc_of_expr _1) (loc_of_expr _3),
	  CMP (C_LE, _3, _1)
	)
# 1211 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_082 =
  fun () ->
    (
# 396 "cparser.mly"
                ( getloc () )
# 1219 "cparser.ml"
     : (Cast.location))

let _menhir_action_083 =
  fun _1 _3 _5 ->
    (
# 355 "cparser.mly"
 ( 
          sup_locator _1 (fst _5), CIF (_3, _5,
					(getloc (), CBLOCK ([], [])))
	)
# 1230 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_084 =
  fun _1 _3 _5 _7 ->
    (
# 360 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1240 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_085 =
  fun _1 _3 _5 _7 ->
    (
# 364 "cparser.mly"
 ( 
          sup_locator _1 (fst _7), CIF (_3, _5, _7)
	)
# 1250 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_086 =
  fun () ->
    (
# 349 "cparser.mly"
                    ( getloc () )
# 1258 "cparser.ml"
     : (Cast.location))

let _menhir_action_087 =
  fun _1 ->
    (
# 187 "cparser.mly"
                              ( _1 )
# 1266 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_088 =
  fun () ->
    (
# 148 "cparser.mly"
                        ( getloc (), STAR_CHR  )
# 1274 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_089 =
  fun _1 ->
    (
# 301 "cparser.mly"
            ( _1 )
# 1282 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_090 =
  fun _1 ->
    (
# 303 "cparser.mly"
            ( loc_of_expr _1, CEXPR _1 )
# 1290 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_091 =
  fun _1 ->
    (
# 305 "cparser.mly"
            ( _1 )
# 1298 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_092 =
  fun _1 ->
    (
# 307 "cparser.mly"
            ( _1 )
# 1306 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_093 =
  fun _1 ->
    (
# 309 "cparser.mly"
            ( _1 )
# 1314 "cparser.ml"
     : (Cast.loc_code))

let _menhir_action_094 =
  fun _1 ->
    (
# 337 "cparser.mly"
          ( [_1] )
# 1322 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_095 =
  fun _1 _2 ->
    (
# 339 "cparser.mly"
          ( _2 :: _1 )
# 1330 "cparser.ml"
     : (Cast.loc_code list))

let _menhir_action_096 =
  fun _1 ->
    (
# 77 "cparser.mly"
                         ( getloc (), _1 )
# 1338 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_097 =
  fun _1 _2 ->
    (
# 79 "cparser.mly"
            ( 
              let l, s = _2 in
              let s2 = _1 in
              (getloc (), s2^s)
            )
# 1350 "cparser.ml"
     : (Cast.location * string))

let _menhir_action_098 =
  fun () ->
    (
# 145 "cparser.mly"
                        ( getloc (), SUB_CHR   )
# 1358 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_099 =
  fun () ->
    (
# 147 "cparser.mly"
                        ( getloc (), TILDE_CHR )
# 1366 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_100 =
  fun () ->
    (
# 296 "cparser.mly"
                  ( TINT )
# 1374 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_101 =
  fun _1 ->
    (
# 297 "cparser.mly"
                           ( TPTR(_1) )
# 1382 "cparser.ml"
     : (Cast.ctyp))

let _menhir_action_102 =
  fun _1 ->
    (
# 115 "cparser.mly"
                             ( _1 )
# 1390 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_103 =
  fun _1 _2 ->
    (
# 117 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_INC, _2) )
# 1398 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_104 =
  fun _1 _2 ->
    (
# 119 "cparser.mly"
 ( sup_locator _1 (loc_of_expr _2), OP1 (M_PRE_DEC, _2) )
# 1406 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_105 =
  fun _1 _2 ->
    (
# 121 "cparser.mly"
 ( 
          let loc, c = _1 in
          let loc' = sup_locator loc (loc_of_expr _2) in
	  match c with
	      ADD_CHR -> _2
	    | SUB_CHR -> loc', OP1 (M_MINUS, _2)
	    | BANG_CHR -> loc', EIF (_2, (loc', CST 0), (loc', CST 1))
            | TILDE_CHR -> loc', OP1 (M_NOT, _2)
            | STAR_CHR -> loc', OP1 (M_DEREF, _2)
            | AMP_CHR -> loc', OP1 (M_ADDR, _2)
	    | _ -> raise (Parse_error (loc, "unknown unary operator"))
	)
# 1425 "cparser.ml"
     : (Cast.loc_expr))

let _menhir_action_106 =
  fun _1 ->
    (
# 136 "cparser.mly"
                    ( _1 )
# 1433 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_107 =
  fun _1 ->
    (
# 137 "cparser.mly"
                    ( _1 )
# 1441 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_108 =
  fun _1 ->
    (
# 138 "cparser.mly"
                    ( _1 )
# 1449 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_109 =
  fun _1 ->
    (
# 139 "cparser.mly"
                    ( _1 )
# 1457 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_110 =
  fun _1 ->
    (
# 140 "cparser.mly"
                    ( _1 )
# 1465 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_111 =
  fun _1 ->
    (
# 141 "cparser.mly"
                    ( _1 )
# 1473 "cparser.ml"
     : (Cast.location * token))

let _menhir_action_112 =
  fun () ->
    (
# 369 "cparser.mly"
                ( getloc () )
# 1481 "cparser.ml"
     : (Cast.location))

let _menhir_print_token : token -> string =
  fun _tok ->
    match _tok with
    | ADD_CHR ->
        "ADD_CHR"
    | AMP_CHR ->
        "AMP_CHR"
    | AND_OP ->
        "AND_OP"
    | BANG_CHR ->
        "BANG_CHR"
    | CLOSE_ANGLE_CHR ->
        "CLOSE_ANGLE_CHR"
    | CLOSE_BRACE_CHR ->
        "CLOSE_BRACE_CHR"
    | CLOSE_PAREN_CHR ->
        "CLOSE_PAREN_CHR"
    | COLON_CHR ->
        "COLON_CHR"
    | COMMA_CHR ->
        "COMMA_CHR"
    | CONSTANT _ ->
        "CONSTANT"
    | DEC_OP ->
        "DEC_OP"
    | DIV_CHR ->
        "DIV_CHR"
    | ELSE ->
        "ELSE"
    | EOF ->
        "EOF"
    | EQ_CHR ->
        "EQ_CHR"
    | EQ_OP ->
        "EQ_OP"
    | FOR ->
        "FOR"
    | GE_OP ->
        "GE_OP"
    | IDENTIFIER _ ->
        "IDENTIFIER"
    | IF ->
        "IF"
    | INC_OP ->
        "INC_OP"
    | INTEGER ->
        "INTEGER"
    | LE_OP ->
        "LE_OP"
    | MOD_CHR ->
        "MOD_CHR"
    | NE_OP ->
        "NE_OP"
    | OPEN_ANGLE_CHR ->
        "OPEN_ANGLE_CHR"
    | OPEN_BRACE_CHR ->
        "OPEN_BRACE_CHR"
    | OPEN_PAREN_CHR ->
        "OPEN_PAREN_CHR"
    | OR_OP ->
        "OR_OP"
    | QUES_CHR ->
        "QUES_CHR"
    | RETURN ->
        "RETURN"
    | SEMI_CHR ->
        "SEMI_CHR"
    | STAR_CHR ->
        "STAR_CHR"
    | STRING_LITERAL _ ->
        "STRING_LITERAL"
    | SUB_CHR ->
        "SUB_CHR"
    | TILDE_CHR ->
        "TILDE_CHR"
    | WHILE ->
        "WHILE"

let _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

include struct
  
  [@@@ocaml.warning "-4-37-39"]
  
  let rec _menhir_run_171_spec_000 : type  ttv_stack. ttv_stack -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      MenhirBox_file _v
  
  let rec _menhir_run_173 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_external_declaration -> _ -> _menhir_box_file =
    fun _menhir_stack _v ->
      let MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_039 _1 _2 in
      _menhir_goto_file _menhir_stack _v _menhir_s
  
  and _menhir_goto_file : type  ttv_stack. ttv_stack -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _v _menhir_s ->
      match _menhir_s with
      | MenhirState172 ->
          _menhir_run_173 _menhir_stack _v
      | MenhirState000 ->
          _menhir_run_171_spec_000 _menhir_stack _v
      | _ ->
          _menhir_fail ()
  
  let rec _menhir_run_003 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 =
            let _1 = _v_0 in
            _menhir_action_043 _1
          in
          (match (_tok : MenhirBasics.token) with
          | OPEN_PAREN_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, MenhirState003, _v_1) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              (match (_tok : MenhirBasics.token) with
              | INTEGER ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_100 () in
                  _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState007 _tok
              | CLOSE_PAREN_CHR ->
                  let _tok = _menhir_lexer _menhir_lexbuf in
                  let _v = _menhir_action_063 () in
                  _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
              | _ ->
                  _eRR ())
          | SEMI_CHR ->
              let _v_4 =
                let _1 = _v_1 in
                _menhir_action_027 _1
              in
              _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_004 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _v = _menhir_action_101 _1 in
      _menhir_goto_type_specifier _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_type_specifier : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState015 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState007 ->
          _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState172 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState000 ->
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_157 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 =
            let _1 = _v_0 in
            _menhir_action_043 _1
          in
          let _1 = _v_1 in
          let _v = _menhir_action_027 _1 in
          _menhir_run_019 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_019 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_024 _1 _2 in
          _menhir_goto_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState000 ->
          _menhir_run_174_spec_000 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState172 ->
          _menhir_run_174_spec_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_168_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_166 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_174_spec_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_037 _1 in
      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
  
  and _menhir_run_172 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_external_declaration (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_100 () in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState172 _tok
      | EOF ->
          let _v = _menhir_action_038 () in
          _menhir_run_173 _menhir_stack _v
      | _ ->
          _eRR ()
  
  and _menhir_run_174_spec_172 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_external_declaration -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_037 _1 in
      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState172 _tok
  
  and _menhir_run_168_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_025 _1 in
      _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_112 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_099 () in
          let _1 = _v_1 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_3 = _menhir_action_098 () in
          let _1 = _v_3 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | STRING_LITERAL _v_5 ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState163
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_6 = _menhir_action_088 () in
          let _1 = _v_6 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | SEMI_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_8 = _menhir_action_086 () in
          let _1 = _v_8 in
          let _v = _menhir_action_034 _1 in
          _menhir_run_147_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RETURN ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_082 () in
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState163
      | OPEN_BRACE_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_11 = _menhir_action_061 () in
          let _1 = _v_11 in
          let _v = _menhir_action_060 _1 in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | INTEGER ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_100 () in
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | INC_OP ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | IF ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_044 () in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | IDENTIFIER _v_16 ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_16 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | FOR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_040 () in
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | CONSTANT _v_20 ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_21 =
            let _1 = _v_20 in
            _menhir_action_022 _1
          in
          let _1 = _v_21 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CLOSE_BRACE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_23 = _menhir_action_014 () in
          let _v_24 =
            let _1 = _v_23 in
            _menhir_action_013 _1
          in
          let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let (_2, _3) = (_v, _v_24) in
          let _v = _menhir_action_018 _1 _2 _3 in
          _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_25 = _menhir_action_011 () in
          let _1 = _v_25 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_27 = _menhir_action_005 () in
          let _1 = _v_27 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_29 = _menhir_action_001 () in
          let _1 = _v_29 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_122 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_whilekw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState123
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState123
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_041 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | STRING_LITERAL _v_4 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState041
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState041
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | IDENTIFIER _v_8 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_8 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | CONSTANT _v_11 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_12 =
            let _1 = _v_11 in
            _menhir_action_022 _1
          in
          let _1 = _v_12 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 = _menhir_action_011 () in
          let _1 = _v_14 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_16 = _menhir_action_005 () in
          let _1 = _v_16 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_18 = _menhir_action_001 () in
          let _1 = _v_18 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_029 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | STRING_LITERAL _v_0 ->
          let _menhir_stack = MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_0 MenhirState029
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_096 _1 in
          _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_string_literal : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState164 ->
          _menhir_run_045_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_045_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_045_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_045_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_045_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_045_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_045_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_045_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_045_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_045_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_045_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_045_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_045_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_045_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_045_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_045_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_045_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_045_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_045_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_045_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState096 ->
          _menhir_run_045_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState094 ->
          _menhir_run_045_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_045_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_045_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState085 ->
          _menhir_run_045_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_045_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_045_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState079 ->
          _menhir_run_045_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState077 ->
          _menhir_run_045_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_045_spec_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState071 ->
          _menhir_run_045_spec_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState064 ->
          _menhir_run_045_spec_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState063 ->
          _menhir_run_045_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_045_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_045_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState051 ->
          _menhir_run_045_spec_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState041 ->
          _menhir_run_045_spec_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState029 ->
          _menhir_run_030 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_045_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
  
  and _menhir_run_048 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_045 () in
          let (_1, _2) = (_v, _v_0) in
          let _v = _menhir_action_071 _1 _2 in
          _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_023 () in
          let (_1, _2) = (_v, _v_1) in
          let _v = _menhir_action_072 _1 _2 in
          _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_102 _1 in
          _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_postfix_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_unary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState064 ->
          _menhir_run_065 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_056 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState051 ->
          _menhir_run_052 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_042_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_042_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState096 ->
          _menhir_run_042_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState094 ->
          _menhir_run_042_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_042_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState085 ->
          _menhir_run_042_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_042_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_042_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState079 ->
          _menhir_run_042_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState077 ->
          _menhir_run_042_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_042_spec_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState071 ->
          _menhir_run_042_spec_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState063 ->
          _menhir_run_042_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_042_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState041 ->
          _menhir_run_042_spec_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_065 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_dec_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_dec_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_104 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_056 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | EQ_CHR ->
          let _menhir_stack = MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState057
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState057
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DIV_CHR | EQ_OP | GE_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_012 _1 in
          _menhir_goto_cast_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_034 : type  ttv_stack. ttv_stack -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState034
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_051 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_inc_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | STRING_LITERAL _v_4 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState051
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState051
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | IDENTIFIER _v_8 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_8 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | CONSTANT _v_11 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_12 =
            let _1 = _v_11 in
            _menhir_action_022 _1
          in
          let _1 = _v_12 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 = _menhir_action_011 () in
          let _1 = _v_14 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_16 = _menhir_action_005 () in
          let _1 = _v_16 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_18 = _menhir_action_001 () in
          let _1 = _v_18 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_053 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | SUB_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | STRING_LITERAL _v_4 ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState054
          | STAR_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | OPEN_PAREN_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState054
          | INC_OP ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | IDENTIFIER _v_8 ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | DEC_OP ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | CONSTANT _v_11 ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | CLOSE_PAREN_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_015 () in
              let (_1, _3) = (_v, _v_14) in
              let _v = _menhir_action_069 _1 _3 in
              _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
          | BANG_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_15 = _menhir_action_011 () in
              let _1 = _v_15 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | AMP_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_17 = _menhir_action_005 () in
              let _1 = _v_17 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | ADD_CHR ->
              let _menhir_stack = MenhirCell1_identifier (_menhir_stack, _menhir_s, _v) in
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_19 = _menhir_action_001 () in
              let _1 = _v_19 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
          | _ ->
              _eRR ())
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | DEC_OP | DIV_CHR | EQ_CHR | EQ_OP | GE_OP | INC_OP | LE_OP | MOD_CHR | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | STAR_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_073 _1 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_064 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_dec_op (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | STRING_LITERAL _v_4 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState064
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState064
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | IDENTIFIER _v_8 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_8 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | CONSTANT _v_11 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_12 =
            let _1 = _v_11 in
            _menhir_action_022 _1
          in
          let _1 = _v_12 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 = _menhir_action_011 () in
          let _1 = _v_14 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_16 = _menhir_action_005 () in
          let _1 = _v_16 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_18 = _menhir_action_001 () in
          let _1 = _v_18 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_064 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_dec_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState064 _tok
  
  and _menhir_run_047_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
  
  and _menhir_goto_primary_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047_spec_051 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_inc_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState051 _tok
  
  and _menhir_run_047_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
  
  and _menhir_run_047_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
  
  and _menhir_goto_cast_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState041 ->
          _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_075_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_075_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_075_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_075_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_075_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_075_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_075_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_075_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_075_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_075_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_075_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_075_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_075_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_075_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_075_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_075_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_075_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_075_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_075_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_075_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_075_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_075_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_075_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState096 ->
          _menhir_run_075_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState094 ->
          _menhir_run_075_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState085 ->
          _menhir_run_075_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_075_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_075_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState079 ->
          _menhir_run_075_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState077 ->
          _menhir_run_075_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_075_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState073 ->
          _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState071 ->
          _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState063 ->
          _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_116 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_operator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_105 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_075_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_062 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let _1 = _v in
          let _v = _menhir_action_002 _1 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_063 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState063
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_063 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState063 _tok
  
  and _menhir_run_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState071
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState071 _tok
  
  and _menhir_run_073 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState073
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_073 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState073 _tok
  
  and _menhir_goto_additive_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState077
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState077
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
          | _ ->
              _eRR ())
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_20 = _menhir_action_099 () in
              let _1 = _v_20 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_22 = _menhir_action_098 () in
              let _1 = _v_22 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | STRING_LITERAL _v_24 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_24 MenhirState079
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_25 = _menhir_action_088 () in
              let _1 = _v_25 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState079
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | IDENTIFIER _v_28 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_28 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | CONSTANT _v_31 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_32 =
                let _1 = _v_31 in
                _menhir_action_022 _1
              in
              let _1 = _v_32 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_34 = _menhir_action_011 () in
              let _1 = _v_34 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_36 = _menhir_action_005 () in
              let _1 = _v_36 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_38 = _menhir_action_001 () in
              let _1 = _v_38 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_087 _1 in
          _menhir_goto_shift_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_047_spec_077 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
  
  and _menhir_run_047_spec_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
  
  and _menhir_goto_shift_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState085 ->
          _menhir_run_086 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState083 ->
          _menhir_run_084 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState081 ->
          _menhir_run_082 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState060 ->
          _menhir_run_061 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_058_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_058_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_058_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_058_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_058_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_058_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_058_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_058_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_058_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_058_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_058_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_058_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_058_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_058_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_058_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_058_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_058_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_058_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_058_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_058_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState096 ->
          _menhir_run_058_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState094 ->
          _menhir_run_058_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_058_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_058_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_058_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_086 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_079 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_relational_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState096 ->
          _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_097 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_029 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState060
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
  
  and _menhir_run_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState081
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
  
  and _menhir_run_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState083
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
  
  and _menhir_run_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState085
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
  
  and _menhir_goto_equality_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | NE_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState094
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState094
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
          | _ ->
              _eRR ())
      | EQ_OP ->
          let _menhir_stack = MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_20 = _menhir_action_099 () in
              let _1 = _v_20 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_22 = _menhir_action_098 () in
              let _1 = _v_22 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | STRING_LITERAL _v_24 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_24 MenhirState096
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_25 = _menhir_action_088 () in
              let _1 = _v_25 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState096
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | IDENTIFIER _v_28 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_28 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | CONSTANT _v_31 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_32 =
                let _1 = _v_31 in
                _menhir_action_022 _1
              in
              let _1 = _v_32 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_34 = _menhir_action_011 () in
              let _1 = _v_34 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_36 = _menhir_action_005 () in
              let _1 = _v_36 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_38 = _menhir_action_001 () in
              let _1 = _v_38 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
          | _ ->
              _eRR ())
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_006 _1 in
          let _1 = _v in
          let _v = _menhir_action_031 _1 in
          let _1 = _v in
          let _v = _menhir_action_046 _1 in
          _menhir_goto_inclusive_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_047_spec_094 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
  
  and _menhir_run_047_spec_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
  
  and _menhir_goto_inclusive_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_099_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_099_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_099_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_099_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_099_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_099_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_099_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_099_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_099_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_099_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_099_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_099_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_099_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_099_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_099_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_099_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_099_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_099_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState107 ->
          _menhir_run_099_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState104 ->
          _menhir_run_099_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_099_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_099_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState090 ->
          _menhir_run_091 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_099_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_089 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_054 _1 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState090
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
  
  and _menhir_goto_logical_or_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | QUES_CHR ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState088
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState088
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
          | _ ->
              _eRR ())
      | OR_OP ->
          let _menhir_stack = MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_20 = _menhir_action_099 () in
              let _1 = _v_20 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_22 = _menhir_action_098 () in
              let _1 = _v_22 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | STRING_LITERAL _v_24 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_24 MenhirState107
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_25 = _menhir_action_088 () in
              let _1 = _v_25 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState107
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | IDENTIFIER _v_28 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_28 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | CONSTANT _v_31 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_32 =
                let _1 = _v_31 in
                _menhir_action_022 _1
              in
              let _1 = _v_32 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_34 = _menhir_action_011 () in
              let _1 = _v_34 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_36 = _menhir_action_005 () in
              let _1 = _v_36 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_38 = _menhir_action_001 () in
              let _1 = _v_38 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_020 _1 in
          _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_047_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_047_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
  
  and _menhir_goto_conditional_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState104 ->
          _menhir_run_105 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_102_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_102_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_102_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_102_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_102_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_102_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_102_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_102_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_102_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_102_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_102_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_102_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_102_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_102_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_102_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState113 ->
          _menhir_run_102_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_102_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_102_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_102_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_102_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_105 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COLON_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_021 _1 _3 _5 in
      _menhir_goto_conditional_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_102_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_148 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_035 _1 in
          _menhir_goto_expression_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState148
      | _ ->
          _eRR ()
  
  and _menhir_goto_expression_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_147_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_147_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_147_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_147_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_147_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_147_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_147_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_147_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_147_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_162_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_162_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_094 _1 in
      _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_112 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_099 () in
          let _1 = _v_1 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_3 = _menhir_action_098 () in
          let _1 = _v_3 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | STRING_LITERAL _v_5 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState164
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_6 = _menhir_action_088 () in
          let _1 = _v_6 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | SEMI_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_8 = _menhir_action_086 () in
          let _1 = _v_8 in
          let _v = _menhir_action_034 _1 in
          _menhir_run_147_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RETURN ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_082 () in
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState164
      | OPEN_BRACE_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_11 = _menhir_action_061 () in
          let _1 = _v_11 in
          let _v = _menhir_action_060 _1 in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | INC_OP ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | IF ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_044 () in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | IDENTIFIER _v_15 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_15 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | FOR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_040 () in
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | CONSTANT _v_19 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_20 =
            let _1 = _v_19 in
            _menhir_action_022 _1
          in
          let _1 = _v_20 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CLOSE_BRACE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_22 = _menhir_action_014 () in
          let _v_23 =
            let _1 = _v_22 in
            _menhir_action_013 _1
          in
          let MenhirCell1_declaration_list (_menhir_stack, _, _2) = _menhir_stack in
          let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let (_3, _4) = (_v, _v_23) in
          let _v = _menhir_action_019 _1 _2 _3 _4 in
          _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_24 = _menhir_action_011 () in
          let _1 = _v_24 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_26 = _menhir_action_005 () in
          let _1 = _v_26 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_28 = _menhir_action_001 () in
          let _1 = _v_28 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_147_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_159 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_statement_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_095 _1 _2 in
      _menhir_goto_statement_list _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statement_list : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_112 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_099 () in
          let _1 = _v_1 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_3 = _menhir_action_098 () in
          let _1 = _v_3 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | STRING_LITERAL _v_5 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState158
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_6 = _menhir_action_088 () in
          let _1 = _v_6 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | SEMI_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_8 = _menhir_action_086 () in
          let _1 = _v_8 in
          let _v = _menhir_action_034 _1 in
          _menhir_run_147_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RETURN ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_082 () in
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState158
      | OPEN_BRACE_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_11 = _menhir_action_061 () in
          let _1 = _v_11 in
          let _v = _menhir_action_060 _1 in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | INC_OP ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | IF ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_044 () in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | IDENTIFIER _v_15 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_15 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | FOR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_040 () in
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | CONSTANT _v_19 ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_20 =
            let _1 = _v_19 in
            _menhir_action_022 _1
          in
          let _1 = _v_20 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CLOSE_BRACE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_22 = _menhir_action_014 () in
          let _v_23 =
            let _1 = _v_22 in
            _menhir_action_013 _1
          in
          let MenhirCell1_open_block (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let (_2, _3) = (_v, _v_23) in
          let _v = _menhir_action_017 _1 _2 _3 in
          _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_24 = _menhir_action_011 () in
          let _1 = _v_24 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_26 = _menhir_action_005 () in
          let _1 = _v_26 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_statement_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_28 = _menhir_action_001 () in
          let _1 = _v_28 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_147_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_129 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | STRING_LITERAL _v_4 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState129
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_050 _1 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState129
      | INC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | IDENTIFIER _v_8 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_8 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | CONSTANT _v_11 ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_12 =
            let _1 = _v_11 in
            _menhir_action_022 _1
          in
          let _1 = _v_12 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 = _menhir_action_011 () in
          let _1 = _v_14 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_16 = _menhir_action_005 () in
          let _1 = _v_16 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_return (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_18 = _menhir_action_001 () in
          let _1 = _v_18 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_jump_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_093 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_162_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_162_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_162_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_094 _1 in
      _menhir_run_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_152 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _6 = _v in
      let _v = _menhir_action_048 _1 _3 _4 _6 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_iteration_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _1 = _v in
      let _v = _menhir_action_092 _1 in
      _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_146 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _4) = _menhir_stack in
      let MenhirCell1_expression_statement (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_forkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_049 _1 _3 _4 _5 _7 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_139 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_083 _1 _3 _5 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_selection_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState154 ->
          _menhir_run_155 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_128_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_128_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_128_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_128_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_128_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_128_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_128_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_128_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_155 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_084 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_128_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_162_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_162_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_128_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_091 _1 in
      _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_126 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_close_paren (_menhir_stack, _, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_whilekw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _5 = _v in
      let _v = _menhir_action_047 _1 _3 _5 in
      _menhir_goto_iteration_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_047_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
  
  and _menhir_run_025 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | WHILE ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_112 () in
          _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | TILDE_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 = _menhir_action_099 () in
          let _1 = _v_1 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | SUB_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_3 = _menhir_action_098 () in
          let _1 = _v_3 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | STRING_LITERAL _v_5 ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState025
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_6 = _menhir_action_088 () in
          let _1 = _v_6 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | SEMI_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_8 = _menhir_action_086 () in
          let _1 = _v_8 in
          let _v = _menhir_action_034 _1 in
          _menhir_run_147_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | RETURN ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_082 () in
          _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | OPEN_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState025
      | OPEN_BRACE_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_11 = _menhir_action_061 () in
          let _1 = _v_11 in
          let _v = _menhir_action_060 _1 in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | INTEGER ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_100 () in
          _menhir_run_157 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | INC_OP ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | IF ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_044 () in
          _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | IDENTIFIER _v_16 ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_16 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | FOR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_040 () in
          _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | DEC_OP ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | CONSTANT _v_20 ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_21 =
            let _1 = _v_20 in
            _menhir_action_022 _1
          in
          let _1 = _v_21 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CLOSE_BRACE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_23 = _menhir_action_014 () in
          let _v_24 =
            let _1 = _v_23 in
            _menhir_action_013 _1
          in
          let (_1, _2) = (_v, _v_24) in
          let _v = _menhir_action_016 _1 _2 in
          _menhir_goto_compound_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | BANG_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_25 = _menhir_action_011 () in
          let _1 = _v_25 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | AMP_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_27 = _menhir_action_005 () in
          let _1 = _v_27 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | ADD_CHR ->
          let _menhir_stack = MenhirCell1_open_block (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_29 = _menhir_action_001 () in
          let _1 = _v_29 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_147_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_162_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_135 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_ifkw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState136
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState136
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
  
  and _menhir_run_140 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_forkw (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState141
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | SEMI_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_7 = _menhir_action_086 () in
              let _1 = _v_7 in
              let _v = _menhir_action_034 _1 in
              _menhir_run_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState141
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | IDENTIFIER _v_10 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_10 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | CONSTANT _v_13 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 =
                let _1 = _v_13 in
                _menhir_action_022 _1
              in
              let _1 = _v_14 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_011 () in
              let _1 = _v_16 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_005 () in
              let _1 = _v_18 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_20 = _menhir_action_001 () in
              let _1 = _v_20 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | STRING_LITERAL _v_4 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState142
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_7 = _menhir_action_086 () in
          let _1 = _v_7 in
          let _v = _menhir_action_034 _1 in
          _menhir_run_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState142
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | IDENTIFIER _v_10 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_10 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | CONSTANT _v_13 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 =
            let _1 = _v_13 in
            _menhir_action_022 _1
          in
          let _1 = _v_14 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_16 = _menhir_action_011 () in
          let _1 = _v_16 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_18 = _menhir_action_005 () in
          let _1 = _v_18 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_20 = _menhir_action_001 () in
          let _1 = _v_20 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression_statement (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_099 () in
          let _1 = _v_0 in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_2 = _menhir_action_098 () in
          let _1 = _v_2 in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | STRING_LITERAL _v_4 ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState143
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_5 = _menhir_action_088 () in
          let _1 = _v_5 in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState143
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | IDENTIFIER _v_8 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v_8 in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | CONSTANT _v_11 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_12 =
            let _1 = _v_11 in
            _menhir_action_022 _1
          in
          let _1 = _v_12 in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_14 = _menhir_action_015 () in
          let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, MenhirState143, _v_14) in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_112 () in
              _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_099 () in
              let _1 = _v_16 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_098 () in
              let _1 = _v_18 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | STRING_LITERAL _v_20 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_20 MenhirState151
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_21 = _menhir_action_088 () in
              let _1 = _v_21 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | SEMI_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_23 = _menhir_action_086 () in
              let _1 = _v_23 in
              let _v = _menhir_action_034 _1 in
              _menhir_run_147_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | RETURN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_082 () in
              _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState151
          | OPEN_BRACE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_26 = _menhir_action_061 () in
              let _1 = _v_26 in
              let _v = _menhir_action_060 _1 in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | IF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_044 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | IDENTIFIER _v_30 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_30 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | FOR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_040 () in
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | CONSTANT _v_34 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_35 =
                let _1 = _v_34 in
                _menhir_action_022 _1
              in
              let _1 = _v_35 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_37 = _menhir_action_011 () in
              let _1 = _v_37 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_39 = _menhir_action_005 () in
              let _1 = _v_39 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_41 = _menhir_action_001 () in
              let _1 = _v_41 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
          | _ ->
              _eRR ())
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_43 = _menhir_action_011 () in
          let _1 = _v_43 in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_45 = _menhir_action_005 () in
          let _1 = _v_45 in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_47 = _menhir_action_001 () in
          let _1 = _v_47 in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
  
  and _menhir_run_147_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
  
  and _menhir_run_047_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
  
  and _menhir_run_047_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
  
  and _menhir_run_047_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_goto_compound_statement : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState022 ->
          _menhir_run_170 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState154 ->
          _menhir_run_156 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_153 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_150_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_150_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_150_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_150_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_150_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_150_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_150_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_170 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_function_declarator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_042 _1 _2 in
      let _1 = _v in
      let _v = _menhir_action_036 _1 in
      _menhir_run_172 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_156 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR, _menhir_box_file) _menhir_cell1_compound_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_compound_statement (_menhir_stack, _, _5) = _menhir_stack in
      let MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _, _3) = _menhir_stack in
      let MenhirCell1_ifkw (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _7 = _v in
      let _v = _menhir_action_085 _1 _3 _5 _7 in
      _menhir_goto_selection_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_153 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | ELSE ->
          let _menhir_stack = MenhirCell1_compound_statement (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | OPEN_BRACE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_061 () in
              let _1 = _v_0 in
              let _v = _menhir_action_060 _1 in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState154 _tok
          | IF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_044 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState154 _tok
          | _ ->
              _eRR ())
      | ADD_CHR | AMP_CHR | BANG_CHR | CLOSE_BRACE_CHR | CONSTANT _ | DEC_OP | FOR | IDENTIFIER _ | IF | INC_OP | OPEN_BRACE_CHR | OPEN_PAREN_CHR | RETURN | SEMI_CHR | STAR_CHR | STRING_LITERAL _ | SUB_CHR | TILDE_CHR | WHILE ->
          let _1 = _v in
          let _v = _menhir_action_089 _1 in
          _menhir_goto_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_150_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_162_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_162_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_159 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_152 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_150_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_089 _1 in
      _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
  
  and _menhir_run_147_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_126 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_147_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_139 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_147_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_090 _1 in
      _menhir_run_146 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression as 'stack) -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _menhir_s ->
      let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, _menhir_s) in
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | TILDE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_099 () in
          let _1 = _v in
          let _v = _menhir_action_109 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | SUB_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_098 () in
          let _1 = _v in
          let _v = _menhir_action_107 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | STRING_LITERAL _v ->
          _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101
      | STAR_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_088 () in
          let _1 = _v in
          let _v = _menhir_action_110 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | OPEN_PAREN_CHR ->
          _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState101
      | INC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_045 () in
          _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | IDENTIFIER _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _1 = _v in
          let _v = _menhir_action_043 _1 in
          _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | DEC_OP ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_023 () in
          _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | CONSTANT _v ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v =
            let _1 = _v in
            _menhir_action_022 _1
          in
          let _1 = _v in
          let _v = _menhir_action_074 _1 in
          _menhir_run_047_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | BANG_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_011 () in
          let _1 = _v in
          let _v = _menhir_action_108 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | AMP_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_005 () in
          let _1 = _v in
          let _v = _menhir_action_111 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | ADD_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_001 () in
          let _1 = _v in
          let _v = _menhir_action_106 _1 in
          _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
  
  and _menhir_run_102_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_102_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
  
  and _menhir_run_102_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
  
  and _menhir_run_102_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
  
  and _menhir_run_124 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState124
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_015 () in
          let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, MenhirState124, _v_0) in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_112 () in
              _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_099 () in
              let _1 = _v_2 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_4 = _menhir_action_098 () in
              let _1 = _v_4 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | STRING_LITERAL _v_6 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState125
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_7 = _menhir_action_088 () in
              let _1 = _v_7 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | SEMI_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_9 = _menhir_action_086 () in
              let _1 = _v_9 in
              let _v = _menhir_action_034 _1 in
              _menhir_run_147_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | RETURN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_082 () in
              _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState125
          | OPEN_BRACE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 = _menhir_action_061 () in
              let _1 = _v_12 in
              let _v = _menhir_action_060 _1 in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | IF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_044 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | IDENTIFIER _v_16 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_16 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | FOR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_040 () in
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | CONSTANT _v_20 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_21 =
                let _1 = _v_20 in
                _menhir_action_022 _1
              in
              let _1 = _v_21 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_23 = _menhir_action_011 () in
              let _1 = _v_23 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_25 = _menhir_action_005 () in
              let _1 = _v_25 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_27 = _menhir_action_001 () in
              let _1 = _v_27 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
  
  and _menhir_run_102_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
  
  and _menhir_run_102_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
  
  and _menhir_run_137 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState137
      | CLOSE_PAREN_CHR ->
          let _menhir_stack = MenhirCell1_CLOSE_PAREN_CHR (_menhir_stack, MenhirState137) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_112 () in
              _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_1 = _menhir_action_099 () in
              let _1 = _v_1 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_3 = _menhir_action_098 () in
              let _1 = _v_3 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | STRING_LITERAL _v_5 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_5 MenhirState138
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_6 = _menhir_action_088 () in
              let _1 = _v_6 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | SEMI_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_8 = _menhir_action_086 () in
              let _1 = _v_8 in
              let _v = _menhir_action_034 _1 in
              _menhir_run_147_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | RETURN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_082 () in
              _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState138
          | OPEN_BRACE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_11 = _menhir_action_061 () in
              let _1 = _v_11 in
              let _v = _menhir_action_060 _1 in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | IF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_044 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | IDENTIFIER _v_15 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_15 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | FOR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_040 () in
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | CONSTANT _v_19 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_20 =
                let _1 = _v_19 in
                _menhir_action_022 _1
              in
              let _1 = _v_20 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_22 = _menhir_action_011 () in
              let _1 = _v_22 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_24 = _menhir_action_005 () in
              let _1 = _v_24 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_26 = _menhir_action_001 () in
              let _1 = _v_26 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
  
  and _menhir_run_102_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
  
  and _menhir_run_102_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
  
  and _menhir_run_102_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
  
  and _menhir_run_102_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
  
  and _menhir_run_102_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
  
  and _menhir_run_144 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState144
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_015 () in
          let _menhir_stack = MenhirCell1_close_paren (_menhir_stack, MenhirState144, _v_0) in
          (match (_tok : MenhirBasics.token) with
          | WHILE ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_112 () in
              _menhir_run_122 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_099 () in
              let _1 = _v_2 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_4 = _menhir_action_098 () in
              let _1 = _v_4 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | STRING_LITERAL _v_6 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_6 MenhirState145
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_7 = _menhir_action_088 () in
              let _1 = _v_7 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | SEMI_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_9 = _menhir_action_086 () in
              let _1 = _v_9 in
              let _v = _menhir_action_034 _1 in
              _menhir_run_147_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | RETURN ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_082 () in
              _menhir_run_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState145
          | OPEN_BRACE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 = _menhir_action_061 () in
              let _1 = _v_12 in
              let _v = _menhir_action_060 _1 in
              _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | IF ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_044 () in
              _menhir_run_135 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | IDENTIFIER _v_16 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_16 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | FOR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_040 () in
              _menhir_run_140 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | CONSTANT _v_20 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_21 =
                let _1 = _v_20 in
                _menhir_action_022 _1
              in
              let _1 = _v_21 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_23 = _menhir_action_011 () in
              let _1 = _v_23 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_25 = _menhir_action_005 () in
              let _1 = _v_25 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_27 = _menhir_action_001 () in
              let _1 = _v_27 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
  
  and _menhir_run_102_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
  
  and _menhir_run_102_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
  
  and _menhir_run_131 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_return as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | SEMI_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_return (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_051 _1 _2 in
          _menhir_goto_jump_statement _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState131
      | _ ->
          _eRR ()
  
  and _menhir_run_102_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_106_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
  
  and _menhir_run_117 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState117
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let MenhirCell1_OPEN_PAREN_CHR (_menhir_stack, _menhir_s) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_076 _2 in
          _menhir_goto_primary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_102_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_114 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_008 _1 _3 in
      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_112 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_argument_expression_list (_menhir_stack, _menhir_s, _v) in
          let _menhir_stack = MenhirCell1_COMMA_CHR (_menhir_stack, MenhirState112) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState113
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState113
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_20 = _menhir_action_015 () in
          let MenhirCell1_identifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let (_3, _4) = (_v, _v_20) in
          let _v = _menhir_action_070 _1 _3 _4 in
          _menhir_goto_postfix_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
  
  and _menhir_run_102_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_111_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_111_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_007 _1 in
      _menhir_run_112 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
  
  and _menhir_run_102_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_109 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_unary_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_010 _1 _3 in
      _menhir_goto_assignment_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_assignment_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState113 ->
          _menhir_run_114 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState054 ->
          _menhir_run_111_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState057 ->
          _menhir_run_109 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState025 ->
          _menhir_run_106_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState163 ->
          _menhir_run_106_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState164 ->
          _menhir_run_106_spec_164 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState158 ->
          _menhir_run_106_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState123 ->
          _menhir_run_106_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState125 ->
          _menhir_run_106_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState136 ->
          _menhir_run_106_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState138 ->
          _menhir_run_106_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState141 ->
          _menhir_run_106_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState142 ->
          _menhir_run_106_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState143 ->
          _menhir_run_106_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState151 ->
          _menhir_run_106_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState145 ->
          _menhir_run_106_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState129 ->
          _menhir_run_106_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState034 ->
          _menhir_run_106_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState088 ->
          _menhir_run_106_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState101 ->
          _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_106_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_032 _1 in
      _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_100 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_expression (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          _menhir_run_101 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState100
      | COLON_CHR ->
          let _menhir_stack = MenhirCell1_COLON_CHR (_menhir_stack, MenhirState100) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | TILDE_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_0 = _menhir_action_099 () in
              let _1 = _v_0 in
              let _v = _menhir_action_109 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | SUB_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_2 = _menhir_action_098 () in
              let _1 = _v_2 in
              let _v = _menhir_action_107 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | STRING_LITERAL _v_4 ->
              _menhir_run_029 _menhir_stack _menhir_lexbuf _menhir_lexer _v_4 MenhirState104
          | STAR_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_5 = _menhir_action_088 () in
              let _1 = _v_5 in
              let _v = _menhir_action_110 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | OPEN_PAREN_CHR ->
              _menhir_run_034 _menhir_stack _menhir_lexbuf _menhir_lexer MenhirState104
          | INC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_045 () in
              _menhir_run_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | IDENTIFIER _v_8 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _1 = _v_8 in
              let _v = _menhir_action_043 _1 in
              _menhir_run_053 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | DEC_OP ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_023 () in
              _menhir_run_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | CONSTANT _v_11 ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_12 =
                let _1 = _v_11 in
                _menhir_action_022 _1
              in
              let _1 = _v_12 in
              let _v = _menhir_action_074 _1 in
              _menhir_run_047_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
          | BANG_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_14 = _menhir_action_011 () in
              let _1 = _v_14 in
              let _v = _menhir_action_108 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | AMP_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_16 = _menhir_action_005 () in
              let _1 = _v_16 in
              let _v = _menhir_action_111 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | ADD_CHR ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v_18 = _menhir_action_001 () in
              let _1 = _v_18 in
              let _v = _menhir_action_106 _1 in
              _menhir_run_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
          | _ ->
              _eRR ())
      | _ ->
          _eRR ()
  
  and _menhir_run_047_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
  
  and _menhir_run_103 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_COMMA_CHR (_menhir_stack, _) = _menhir_stack in
      let MenhirCell1_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_033 _1 _3 in
      _menhir_goto_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState163 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_148 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_144 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_137 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_131 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_124 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_117 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_100 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_102_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_106_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_102_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_009 _1 in
      _menhir_run_103 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_099_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
  
  and _menhir_run_099_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_099_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
  
  and _menhir_run_099_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
  
  and _menhir_run_099_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
  
  and _menhir_run_099_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
  
  and _menhir_run_099_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
  
  and _menhir_run_099_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
  
  and _menhir_run_099_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
  
  and _menhir_run_099_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
  
  and _menhir_run_099_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
  
  and _menhir_run_099_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
  
  and _menhir_run_099_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
  
  and _menhir_run_099_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
  
  and _menhir_run_099_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
  
  and _menhir_run_099_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
  
  and _menhir_run_099_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
  
  and _menhir_run_099_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
  
  and _menhir_run_108 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | AND_OP ->
          let _menhir_stack = MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_090 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_logical_or_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_055 _1 _3 in
          _menhir_goto_logical_or_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_099_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
  
  and _menhir_run_099_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
  
  and _menhir_run_099_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_052 _1 in
      _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_091 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_logical_and_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_053 _1 _3 in
      _menhir_goto_logical_and_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_logical_and_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState107 ->
          _menhir_run_108 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_089 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_095 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let MenhirCell1_equality_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_030 _1 _3 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_059 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | OPEN_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_060 _menhir_stack _menhir_lexbuf _menhir_lexer
      | LE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_081 _menhir_stack _menhir_lexbuf _menhir_lexer
      | GE_OP ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_083 _menhir_stack _menhir_lexbuf _menhir_lexer
      | CLOSE_ANGLE_CHR ->
          let _menhir_stack = MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_085 _menhir_stack _menhir_lexbuf _menhir_lexer
      | AND_OP | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | NE_OP | OR_OP | QUES_CHR | SEMI_CHR ->
          let _1 = _v in
          let _v = _menhir_action_028 _1 in
          _menhir_goto_equality_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_084 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_081 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_082 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_080 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_061 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_relational_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_078 _1 _3 in
      _menhir_goto_relational_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_058_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_058_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
  
  and _menhir_run_058_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState025 _tok
  
  and _menhir_run_058_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
  
  and _menhir_run_058_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
  
  and _menhir_run_058_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
  
  and _menhir_run_058_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
  
  and _menhir_run_058_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
  
  and _menhir_run_058_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
  
  and _menhir_run_058_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
  
  and _menhir_run_058_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
  
  and _menhir_run_058_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
  
  and _menhir_run_058_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
  
  and _menhir_run_058_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
  
  and _menhir_run_058_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
  
  and _menhir_run_058_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
  
  and _menhir_run_058_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
  
  and _menhir_run_058_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
  
  and _menhir_run_058_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
  
  and _menhir_run_058_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
  
  and _menhir_run_058_spec_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_097 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
  
  and _menhir_run_058_spec_094 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_095 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
  
  and _menhir_run_058_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
  
  and _menhir_run_058_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_058_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_077 _1 in
      _menhir_run_059 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
  
  and _menhir_run_075_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_075_spec_164 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState164 _tok
  
  and _menhir_run_075_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState158 _tok
  
  and _menhir_run_075_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
  
  and _menhir_run_075_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState125 _tok
  
  and _menhir_run_075_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState136 _tok
  
  and _menhir_run_075_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState138 _tok
  
  and _menhir_run_075_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState141 _tok
  
  and _menhir_run_075_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState142 _tok
  
  and _menhir_run_075_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState143 _tok
  
  and _menhir_run_075_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState151 _tok
  
  and _menhir_run_075_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState145 _tok
  
  and _menhir_run_075_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState129 _tok
  
  and _menhir_run_075_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState034 _tok
  
  and _menhir_run_075_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState113 _tok
  
  and _menhir_run_075_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState054 _tok
  
  and _menhir_run_075_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState057 _tok
  
  and _menhir_run_075_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState107 _tok
  
  and _menhir_run_075_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState088 _tok
  
  and _menhir_run_075_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState104 _tok
  
  and _menhir_run_075_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState101 _tok
  
  and _menhir_run_075_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState090 _tok
  
  and _menhir_run_075_spec_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState096 _tok
  
  and _menhir_run_075_spec_094 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState094 _tok
  
  and _menhir_run_075_spec_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState085 _tok
  
  and _menhir_run_075_spec_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState083 _tok
  
  and _menhir_run_075_spec_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState081 _tok
  
  and _menhir_run_075_spec_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState079 _tok
  
  and _menhir_run_080 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_003 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_075_spec_077 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState077 _tok
  
  and _menhir_run_078 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_063 _menhir_stack _menhir_lexbuf _menhir_lexer
      | MOD_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_071 _menhir_stack _menhir_lexbuf _menhir_lexer
      | DIV_CHR ->
          let _menhir_stack = MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _v) in
          _menhir_run_073 _menhir_stack _menhir_lexbuf _menhir_lexer
      | ADD_CHR | AND_OP | CLOSE_ANGLE_CHR | CLOSE_PAREN_CHR | COLON_CHR | COMMA_CHR | EQ_OP | GE_OP | LE_OP | NE_OP | OPEN_ANGLE_CHR | OR_OP | QUES_CHR | SEMI_CHR | SUB_CHR ->
          let MenhirCell1_additive_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _3 = _v in
          let _v = _menhir_action_004 _1 _3 in
          _menhir_goto_additive_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_075_spec_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_056 _1 in
      _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState060 _tok
  
  and _menhir_run_074 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_058 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_goto_multiplicative_expression : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState079 ->
          _menhir_run_080 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState077 ->
          _menhir_run_078 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState163 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState164 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState025 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState158 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState151 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState145 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState143 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState142 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState141 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState138 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState136 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState125 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState129 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState123 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState034 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState113 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState054 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState107 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState104 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState101 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState096 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState094 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState090 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState088 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState057 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState085 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState083 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState081 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | MenhirState060 ->
          _menhir_run_062 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_072 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_059 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_070 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_multiplicative_expression (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_057 _1 _3 in
      _menhir_goto_multiplicative_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_052 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_inc_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_inc_op (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_103 _1 _2 in
      _menhir_goto_unary_expression _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_042_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_094 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_077 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_073 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_074 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_072 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_063 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_070 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_075_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_042_spec_041 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_012 _1 in
      _menhir_run_116 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_163 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState163 _tok
  
  and _menhir_run_045_spec_158 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_statement_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_158 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_025 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_open_block -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_151 : type  ttv_stack. ((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_151 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_145 : type  ttv_stack. (((((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_145 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_143 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_143 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_142 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_forkw, _menhir_box_file) _menhir_cell1_expression_statement -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_142 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_141 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_forkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_141 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_138 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_ifkw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_CLOSE_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_138 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_136 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_ifkw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_136 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_129 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_return -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_129 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_125 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_whilekw, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_close_paren -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_125 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_123 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_123 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_whilekw -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState123 _tok
  
  and _menhir_run_045_spec_034 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_OPEN_PAREN_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_034 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_113 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_argument_expression_list, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_113 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_054 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_054 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_107 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_107 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_104 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COLON_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_104 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_101 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_expression, _menhir_box_file) _menhir_cell1_COMMA_CHR -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_101 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_096 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_096 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_094 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_equality_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_094 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_090 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_and_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_090 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_088 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_logical_or_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_088 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_085 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_085 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_083 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_083 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_081 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_081 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_079 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_079 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_077 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_additive_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_077 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_073 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_073 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_071 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_071 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_064 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_dec_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_064 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_063 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_multiplicative_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_063 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_060 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_relational_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_060 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_057 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_expression -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_057 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_051 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_inc_op -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_051 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_045_spec_041 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_075 _1 in
      _menhir_run_047_spec_041 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
  
  and _menhir_run_047_spec_041 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_unary_operator -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_068 _1 in
      _menhir_run_048 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState041 _tok
  
  and _menhir_run_030 : type  ttv_stack. (ttv_stack, _menhir_box_file) _menhir_cell1_STRING_LITERAL -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_STRING_LITERAL (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_097 _1 _2 in
      _menhir_goto_string_literal _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_166 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_open_block, _menhir_box_file) _menhir_cell1_declaration_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_declaration_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _2 = _v in
      let _v = _menhir_action_026 _1 _2 in
      _menhir_run_163 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  and _menhir_run_009 : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      let _menhir_stack = MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | STAR_CHR ->
          _menhir_run_004 _menhir_stack _menhir_lexbuf _menhir_lexer
      | IDENTIFIER _v_0 ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_1 =
            let _1 = _v_0 in
            _menhir_action_043 _1
          in
          let _1 = _v_1 in
          let _v = _menhir_action_027 _1 in
          let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
          let _2 = _v in
          let _v = _menhir_action_062 _1 _2 in
          _menhir_goto_parameter_declaration _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameter_declaration : type  ttv_stack. ttv_stack -> _ -> _ -> _ -> (ttv_stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match _menhir_s with
      | MenhirState007 ->
          _menhir_run_017_spec_007 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | MenhirState015 ->
          _menhir_run_016 _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _menhir_fail ()
  
  and _menhir_run_017_spec_007 : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let _1 = _v in
      let _v = _menhir_action_065 _1 in
      _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState007 _tok
  
  and _menhir_run_014 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier as 'stack) -> _ -> _ -> _ -> ('stack, _menhir_box_file) _menhir_state -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok ->
      match (_tok : MenhirBasics.token) with
      | COMMA_CHR ->
          let _menhir_stack = MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _v) in
          let _tok = _menhir_lexer _menhir_lexbuf in
          (match (_tok : MenhirBasics.token) with
          | INTEGER ->
              let _tok = _menhir_lexer _menhir_lexbuf in
              let _v = _menhir_action_100 () in
              _menhir_run_009 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState015 _tok
          | _ ->
              _eRR ())
      | CLOSE_PAREN_CHR ->
          let _1 = _v in
          let _v = _menhir_action_067 _1 in
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _2 = _v in
          let _v = _menhir_action_064 _2 in
          _menhir_goto_parameter_declarator _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok
      | _ ->
          _eRR ()
  
  and _menhir_goto_parameter_declarator : type  ttv_stack. ((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_identifier (_menhir_stack, _, _2) = _menhir_stack in
      let MenhirCell1_type_specifier (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_041 _1 _2 _3 in
      let _menhir_stack = MenhirCell1_function_declarator (_menhir_stack, _menhir_s, _v) in
      match (_tok : MenhirBasics.token) with
      | OPEN_BRACE_CHR ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v_0 = _menhir_action_061 () in
          let _1 = _v_0 in
          let _v = _menhir_action_060 _1 in
          _menhir_run_025 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState022 _tok
      | _ ->
          _eRR ()
  
  and _menhir_run_016 : type  ttv_stack. (((ttv_stack, _menhir_box_file) _menhir_cell1_type_specifier, _menhir_box_file) _menhir_cell1_identifier, _menhir_box_file) _menhir_cell1_parameter_list -> _ -> _ -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer _v _tok ->
      let MenhirCell1_parameter_list (_menhir_stack, _menhir_s, _1) = _menhir_stack in
      let _3 = _v in
      let _v = _menhir_action_066 _1 _3 in
      _menhir_run_014 _menhir_stack _menhir_lexbuf _menhir_lexer _v _menhir_s _tok
  
  let rec _menhir_run_000 : type  ttv_stack. ttv_stack -> _ -> _ -> _menhir_box_file =
    fun _menhir_stack _menhir_lexbuf _menhir_lexer ->
      let _tok = _menhir_lexer _menhir_lexbuf in
      match (_tok : MenhirBasics.token) with
      | INTEGER ->
          let _tok = _menhir_lexer _menhir_lexbuf in
          let _v = _menhir_action_100 () in
          _menhir_run_003 _menhir_stack _menhir_lexbuf _menhir_lexer _v MenhirState000 _tok
      | EOF ->
          let _v = _menhir_action_038 () in
          _menhir_run_171_spec_000 _menhir_stack _v
      | _ ->
          _eRR ()
  
end

let file =
  fun _menhir_lexer _menhir_lexbuf ->
    let _menhir_stack = () in
    let MenhirBox_file v = _menhir_run_000 _menhir_stack _menhir_lexbuf _menhir_lexer in
    v

# 452 "cparser.mly"
  

# 7516 "cparser.ml"
