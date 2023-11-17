(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1
*
* ABEL VERLEY
*__________________________________________________________________
*)

(* Ce programme effectue une verification de type sans produire un arbre syntaxique type *)

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
  val get_type : string -> ctyp
  val get_args_types : string -> ctyp list
end

(* Est ce que ça ne serait pas mieux avec une table de Hashage ? *)
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

  let get_type_decl d = match d with
    | CDECL (_, _, typ) -> typ
    | CFUN (_, _, _, typ, _) -> typ

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

  let get_type s = get_type_decl (get_elmt s)

  let get_args_types s = 
    match get_elmt s with 
    | CDECL _ -> failwith "Trying to get arguments a variable declaration"
    | CFUN (_, _, l, _, _) -> List.map get_type_decl l
end





(*______________ FUNCTIONS FOR TYPE CHECKING ______________*)
open Env
exception Type_Error of string

let check_mon_op mon_op typ  = 
  match mon_op with
    | M_MINUS | M_NOT | M_POST_INC | M_POST_DEC | M_PRE_INC | M_PRE_DEC  -> 
      begin 
        match typ with 
        | TINT -> typ
        | _ -> raise (Type_Error "Types are not corresponding in MON_OP")
      end
    | M_DEREF | M_ADDR  -> 
      begin
        match typ with
        | TPTR _ -> typ
        | _ -> raise (Type_Error "Types are not corresponding in MON_OP") (* TO MODIFY *)
      end


let check_bin_op bin_op typ1 typ2 = 
  match bin_op with
    | S_MUL | S_DIV | S_MOD -> 
      if (typ1 = TINT && typ2 = TINT ) then
        TINT
      else 
        raise (Type_Error "Types are not corresponding in BIN_OP ") (* TO MODIFY *)
    | S_ADD -> 
      begin
        match typ1, typ2 with
        | TINT, TINT -> TINT
        | TPTR t, TINT -> TPTR t
        | _ -> raise (Type_Error "Types are not corresponding in BIN_OP ") (* TO MODIFY *)
      end 
    | S_SUB ->
      begin
        match typ1, typ2 with
        | TINT, TINT -> TINT
        | TPTR t, TINT -> TPTR t
        | TPTR t1, TPTR t2 when t1 = t2 -> TPTR t1
        | _ -> raise (Type_Error "Types are not corresponding in BIN_OP ") (* TO MODIFY *)
      end 

let check_cmp cmp_op typ1 typ2 = 
  match cmp_op with
  | C_LT   | C_LE ->
    if (typ1 = TINT && typ2 = TINT) then TINT
    else raise (Type_Error "Types are not corresponding in CMP ") (* TO MODIFY *)
  | C_EQ -> 
    if (typ1 = typ2) then typ1
    else raise (Type_Error "Types are not corresponding in CMP ") (* TO MODIFY *)


let rec check_loc_expr le =
  let (_, exp) = le in
  check_expr exp
(* Rajouter la verification de valeur gauche *)
(* Rajouter la verification de presence d'un return *)
(* Rajouter le traitement d'un pointeur null ? *)
(* TO DO *)
and check_expr exp = match exp with
  | VAR s -> get_type s
  | CST n -> TINT
  | STRING s -> TPTR TINT
  | SET_VAR (s, le) -> 
    let typ1 = check_loc_expr le in
    let typ2 = get_type s in
    if typ1 = typ2 then 
      typ1
    else
      raise (Type_Error "Types are not corresponding in SET_VAR") (* TO MODIFY *)
  | SET_VAL (s, le) -> 
    let typ1 = check_loc_expr le in
    begin
    match get_type s with 
    |TPTR t when t = typ1 -> typ1
    |_ -> raise(Type_Error "Types are not corresponding in SET_VAL") (* TO MODIFY *)
    end 
  | CALL (s, le_list) -> 
    let le_typs = List.map check_loc_expr le_list in
    let args_typs = get_args_types s in
    if le_typs = args_typs then
      get_type s
    else
      raise (Type_Error "Types are not correspondong in CALL") (* TO MODIFY *)
  | OP1 (m_op, le) -> 
    let typ = check_loc_expr le in
    check_mon_op m_op typ
  | OP2 (b_op, le1, le2) ->
    let typ1 = check_loc_expr le1 in
    let typ2 = check_loc_expr le2 in 
    check_bin_op b_op typ1 typ2
  | CMP (cmp_op, le1, le2) ->
    let typ1 = check_loc_expr le1 in
    let typ2 = check_loc_expr le2 in
    check_cmp cmp_op typ1 typ2
  | EIF (le1, le2, le3) -> 
    let typ1 = check_loc_expr le1 in
    let typ2 = check_loc_expr le2 in
    let typ3 = check_loc_expr le3 in
    begin
      match typ1, typ2, typ3 with
      | TINT, t1, t2 when t1 = t2 -> t1
      | _ -> raise (Type_Error "Types are not correspondong in EIF") (* TO MODIFY *)
    end
  | ESEQ le_list ->
    let l = List.rev (List.map check_loc_expr le_list) in
    begin 
      match l with
      | [] -> TINT
      | h::t -> h 
    end


let rec check_var_declaration v = match v with
 | CDECL (pos, name, t) -> 
    push (var_declaration_loc_create v false)
 | CFUN (pos, name, args, t, l_code) -> 
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


(* TO DO : checkfile with global variable *)