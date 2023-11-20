(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1
*
* ABEL VERLEY
*__________________________________________________________________
*)

(* Cette Interface définit un arbre syntaxique type a l'aide de l'interface Cast *)
(* On ajoute un typage sur les noeuds expression es code *)

open Cast


type typ = Tint | Tptr of typ

type typ_opt = ctyp option
  
type typ_expr = typ_opt * expr_bis
and expr_bis =
  | VAR of string (** une variable --- toujours de type int. *)
  | CST of int (** une constante entiere. *)
  | STRING of string (** une constante chaine. *)
  | SET_VAR of string * typ_expr (** affectation x=e. *)
  | SET_VAL of string * typ_expr (** affectation *x=e. *)
  | CALL of string * typ_expr list (** appel de fonction f(e1,...,en) *)
  | OP1 of mon_op * typ_expr
      (** OP1(mop, e) dénote -e, ~e, e++, e--, ++e, --e, &e ou *e. *)
  | OP2 of bin_op * typ_expr * typ_expr
      (** OP2(bop,e,e') dénote e*e', e/e', e%e',
                               e+e', e-e'. *)
  | CMP of cmp_op * typ_expr * typ_expr
      (** CMP(cop,e,e') vaut e<e', e<=e', ou e==e' *)
  | EIF of typ_expr * typ_expr * typ_expr
      (** EIF(e1,e2,e3) est e1?e2:e3 *)
  | ESEQ of typ_expr list
      (** e1, ..., en [sequence, analogue a e1;e2 au niveau code];
        si n=0, represente skip. *)

  
type typ_var_declaration =
  | CDECL of string * ctyp
      (** declaration de variable de type ctyp. *)
  | CFUN of string * typ_var_declaration list * ctyp * typ_code
      (** fonction avec ses arguments, le type du resultat et son code. *)
and typ_code = typ_opt * code_bis
and code_bis =
    CBLOCK of typ_var_declaration list * typ_code list (** { declarations; code; } *)
  | CEXPR of typ_expr (** une expression e; vue comme instruction. *)
  | CIF of typ_expr * typ_code * typ_code (** if (e) c1; else c2; *)
  | CWHILE of typ_expr * typ_code (** while (e) c1; *)
  | CRETURN of typ_expr option (** return; ou return (e); *)
  

