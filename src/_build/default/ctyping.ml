(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1

*)

open Cast

(*______________ DEFINITION OF AN ENVIRONMENT (Stack)_____________*)
module type Env_s = 
sig
  type var_declaration_loc
  type env_t
  exception Already_Declared_Error of string
  val var_declaration_loc_create : var_declaration -> bool -> var_declaration_loc
  val env : env_t 
  val push : var_declaration_loc -> unit
  val pop : unit  -> var_declaration
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
      |CDECL(_, name1, _), CDECL(_, name2, _) when name1 = name2 -> if elmt.global then raise (Already_Declared_Error "Variable at line was already defined global at line") 
      (* Error if a function is already declared*)
      |CFUN (_, name1, _, _, _), CFUN(_, name2, _,_, _) when name1 = name2 -> raise (Already_Declared_Error "Function was already defined")
      |_ -> ()
    in 
    Stack.iter name_identification env;
    Stack.push v env

  let pop () = (Stack.pop env).var
end

open Env




(* FUNCTIONS FOR TYPE CHECKING *)

let rec check_var_declaration v = match v with
 |CDECL (pos, name, t) -> push (var_declaration_loc_create v false)
 |CFUN (pos, name, args, t, l_code) -> List.iter check_var_declaration args;
                                      check_loc_code l_code

and check_loc_code l_code = 
  let (_,c) = l_code in
  match c with 
    | CBLOCK (decs, codes) -> List.iter 
    | CEXPR
    | CIF
    | CWHILE
    | CRETURN
