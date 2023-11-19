(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1
*
* ABEL VERLEY
*__________________________________________________________________
*)

(* Cette Interface définit un arbre syntaxique type *)


open Cast


type typ = Tint | Tptr of typ


type ctyp =
    TINT
  | TPTR of ctyp

type typ_opt = ctyp option

type location = Lexing.position * Lexing.position

type mon_op_t = mon_op * typ

type bin_op_t = bin_op * typ


type cmp_op_t = cmp_op * typ

type loc_expr = location * expr
and expr =

  | VAR of string (** une variable --- toujours de type int. *)
  | CST of int (** une constante entiere. *)
  | STRING of string (** une constante chaine. *)
  | SET_VAR of string * loc_expr (** affectation x=e. *)
  | SET_VAL of string * loc_expr (** affectation *x=e. *)
  | CALL of string * loc_expr list (** appel de fonction f(e1,...,en) *)

  | OP1 of mon_op * loc_expr
    (** OP1(mop, e) dénote -e, ~e, e++, e--, ++e, --e, &e ou *e. *)
  | OP2 of bin_op * loc_expr * loc_expr
    (** OP2(bop,e,e') dénote e*e', e/e', e%e',
                             e+e', e-e'. *)
  | CMP of cmp_op * loc_expr * loc_expr
    (** CMP(cop,e,e') vaut e<e', e<=e', ou e==e' *)
  | EIF of loc_expr * loc_expr * loc_expr
    (** EIF(e1,e2,e3) est e1?e2:e3 *)
  | ESEQ of loc_expr list
    (** e1, ..., en [sequence, analogue a e1;e2 au niveau code];
      si n=0, represente skip. *)


type var_declaration =
  | CDECL of location * string * ctyp
    (** declaration de variable de type ctyp. *)
  | CFUN of location * string * var_declaration list * ctyp * loc_code
    (** fonction avec ses arguments, le type du resultat et son code. *)
and loc_code = location * code
and code =
    CBLOCK of var_declaration list * loc_code list (** { declarations; code; } *)
  | CEXPR of loc_expr (** une expression e; vue comme instruction. *)
  | CIF of loc_expr * loc_code * loc_code (** if (e) c1; else c2; *)
  | CWHILE of loc_expr * loc_code (** while (e) c1; *)
  | CRETURN of loc_expr option (** return; ou return (e); *)

val cline : int ref
val ccol : int ref
val oldcline : int ref
val oldccol : int ref
val cfile : string ref

val getloc : unit -> location 

val loc_of_expr : location * 'a -> location
val e_of_expr : loc_expr -> expr

val string_of_expr : expr -> string
val string_of_loc_expr : loc_expr -> string

