(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1
*
* ABEL VERLEY
*__________________________________________________________________
*)

open Cast

(*______________ DEFINITION OF AN ENVIRONMENT (Stack) ______________ *)
module type Env_s = 
sig
  type var_declaration_loc
  type env_t
  exception Already_Declared_Error of string
  val var_declaration_loc_create : var_declaration -> bool -> var_declaration_loc
  val env : env_t 
  val push : var_declaration_loc -> unit
  val pop : unit  -> var_declaration
  val get_elmt : string -> var_declaration
end

module Env : Env_s = 
struct 
  type var_declaration_loc = {var : var_declaration; global : bool} (* Status is True iff the declaration is global *)
  type env_t = var_declaration_loc Stack.t 
  exception Already_Declared_Error of string

  let var_declaration_loc_create v b = {var = v; global = b}
  
  let env = Stack.create()

  let push v =
    let name_identification elmt = match v.var, elmt.var with
      (* Error if a global variable is already declared *)
      | CDECL(_, name1, _), CDECL(_, name2, _) when name1 = name2 -> 
        if elmt.global then raise (Already_Declared_Error "Variable at line was already defined global at line") 
      (* Error if a function is already declared*)
      | CFUN (_, name1, _, _, _), CFUN(_, name2, _,_, _) when name1 = name2 -> 
        raise (Already_Declared_Error "Function was already defined")
      |_ -> ()
    in 
    Stack.iter name_identification env;
    Stack.push v env

  let pop () = (Stack.pop env).var

  let get_elmt s = 
    let rec get_elmt_aux stack = 
      let v_loc = Stack.pop stack in
      match v_loc.var with
      | CDECL (loc, name, t) when name = s -> CDECL (loc, name, t)
      | CFUN (loc, name, args, t, code) when name = s -> CFUN (loc, name, args, t, code)
      | _ when not (Stack.is_empty env) -> let e = get_elmt_aux stack in Stack.push v_loc stack; e
      | _ -> failwith "Name absent in env"
    in 
    get_elmt_aux env
end

open Env




(*______________ FUNCTIONS FOR TYPE CHECKING ______________*)

let check_loc_expr le =
  let (_, exp) = le in
  check_expr exp
(* TO DO *)
and check_expr exp = match exp with
  | VAR s -> TINT
  | CST n -> TINT
  | STRING s -> TPTR TINT
  | SET_VAR (s, le) -> 
  | SET_VAL (s, le) ->
  | CALL (s, le_list) ->
  | OP1 (m_op, le) ->
  | OP2 (b_op, le1, le2) ->
  | CMP (c_op, le1, le2) ->
  | EIF (le1, le2, le3) ->
  | ESEQ le_list


let rec check_var_declaration v = match v with
 |CDECL (pos, name, t) -> 
    push (var_declaration_loc_create v false)
 |CFUN (pos, name, args, t, l_code) -> 
    List.iter check_var_declaration args; 
    check_loc_code l_code

and check_loc_code l_code = 
  let (_,c) = l_code in
  check_code c
and check_code code = match code with 
    | CBLOCK (decs, codes) -> 
      List.iter check_var_declaration decs; 
      List.iter check_code codes
    | CEXPR e -> 
      check_loc_expr e
    | CIF (le, lc1, lc2) -> 
      check_loc_expr le;
      check_loc_code lc1;
      check_loc_code lc2
    | CWHILE (le, lc) ->
      check_loc_expr le;
      check_loc_code lc
    | CRETURN le_opt -> (* TO DO *)