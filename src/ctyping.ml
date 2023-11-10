open Cast

(* Definition of the Environment (Stack)*)
module type Env_s = 
sig
  type var_declaration_loc
  type env_t
  exception Already_Declared_Error of string
  val env : env_t 
  val push : var_declaration_loc -> unit
  val pop : unit  -> unit
end

module Env : Env_s = 
struct 
  type var_declaration_loc = {var : var_declaration; status : bool} (* Status is True iff the declaration is global *)
  type env_t = var_declaration_loc Stack.t 
  exception Already_Declared_Error of string
  let env = Stack.create()
  let push v =
    let name_identification elmt = match v.var, elmt.var with
      |CDECL(_, name1, _), CDECL(_, name2, _) when name1 = name2 -> if v.status then raise (Already_Declared_Error "Global variable was already defined") (*Ajouter les lignes de déclatation ... *)
      |CFUN (_, name1, _, _, _), CFUN(_, name2, _,_, _) when name1 = name2 -> raise (Already_Declared_Error "Function was already defined")
      |_ -> ()
    in 
    Stack.iter name_identification env;
    Stack.push v env
  
  let pop () = if (Stack.is_empty env) then 
    let _ = Stack.pop env in ()
end

open Env

