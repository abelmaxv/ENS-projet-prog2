(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 1
*
* ABEL VERLEY
*__________________________________________________________________
*)

(* Ce programme effectue une verification de type et renvoie un arbre syntaique type *)

(* STRUCTURE DU PROGRAMME : 
   I- DEFINITION D'UNE STRUCTURE DE DONNEE POUR L'ENVIRONNEMENT
   II- PORGRAMME DE VERIFICATION DE TYPE
   II.1 - FONCTIONS AUXILIERES DE VERIFICATION POUR LES EXPRESSIONS
   II.2 - TRAITEMENT DES EXPRESSIONS ET GENERATION TAST 
   II.3 - FONCTIONS AUXILIERES DE VERIFICATION POUR LES BLOCS DE CODE
   II.4 - TRAITEMENT DES BLOCS DE CODE ET GENERATION TAST*)
   

open Cast
open List


(*______________ DEFINITION OF AN ENVIRONMENT (Stack) ______________ *)

module type Env_s = 
sig
  type var_declaration_loc
  type env_t
  exception Declaration_Error of location * string
  val var_declaration_loc_create : var_declaration -> bool -> var_declaration_loc
  val env : env_t 
  val push : var_declaration_loc -> unit
  val pop : location  -> unit
  val pop_mult : int -> location -> unit
  val get_elmt : string -> location -> var_declaration
  val get_type : string -> location -> ctyp
  val get_args_types : string -> location -> ctyp list
  val is_function : string -> location -> bool
end



module Env : Env_s = 
struct 
  type var_declaration_loc = {var : var_declaration; global : bool} (* Status is True iff the declaration is global *)
  type env_t = var_declaration_loc Stack.t 
  exception Declaration_Error of location * string

  let var_declaration_loc_create v b = {var = v; global = b}
  
  let env = Stack.create()

  let push v =
    let name_identification elmt = match v.var, elmt.var with
      | CDECL(l, name1, _), CDECL(_, name2, _) when name1 = name2 -> 
        if elmt.global then raise (Declaration_Error (l, "Variable at line was already declared global")) 
      | CFUN (l, name1, _, _, _), CFUN(_, name2, _,_, _) when name1 = name2 -> 
        raise (Declaration_Error (l,"Function was already declared"))
      |_ -> ()
    in 
    Stack.iter name_identification env;
    Stack.push v env

  let pop l = 
    try
      let _ = Stack.pop env in ()
    with 
    | Stack.Empty -> raise (Declaration_Error (l, "Issue when managing environement"))

  let pop_mult n l = 
    for i = 0 to (n-1) do 
      pop l 
    done

  let get_type_decl d = match d with
    | CDECL (_, _, typ) -> typ
    | CFUN (_, _, _, typ, _) -> typ

  let get_elmt s loc = 
    let rec get_elmt_aux stack = 
      let v_loc = Stack.pop stack in
      match v_loc.var with
      | CDECL (loc, name, t) when name = s -> push v_loc ; CDECL (loc, name, t)
      | CFUN (loc, name, args, t, code) when name = s -> push v_loc; CFUN (loc, name, args, t, code)
      | _ when not (Stack.is_empty env) -> let e = get_elmt_aux stack in Stack.push v_loc stack; e
      | _ -> raise (Declaration_Error (loc, "Name was not declared"))
    in 
    get_elmt_aux env

  let get_type s loc = get_type_decl (get_elmt s loc)

  let get_args_types s loc = 
    match (get_elmt s loc)  with 
    | CDECL _ -> raise (Declaration_Error (loc, "Trying to get arguments a variable declaration"))
    | CFUN (_, _, l, _, _) -> List.map get_type_decl l

  let is_function name l = 
    let v = get_elmt name l in
    match v with 
    | CDECL _ -> false
    | CFUN _ -> true 
end














(*______________ FUNCTIONS FOR TYPE CHECKING ______________*)
open Env
open Tast
exception Type_Error of location * string

(*CHECKING FUNCTIONS FOR EXPRESSIONS*)


let check_set_var s taste l = 
  let (t_opt, _) = taste in
  let typ = get_type s l in
  if is_function s l then 
    raise (Type_Error (l, "Impossible to do an assignment on a function "))
  else
    begin
      match typ, t_opt with
      | t1, Some t2 when t1 <> t2 -> raise (Type_Error (l, "Expression on the right of affectation don't have the right type"))
      | _ -> () 
    end



let check_set_val s taste l =
  let (t_opt, _) = taste in
  let typ = get_type s l in
  if is_function s l then 
    raise (Type_Error (l, "Impossible to do an assignment on a function "))
  else
    begin
      match typ, t_opt with
      | TINT, _ -> raise (Type_Error (l, "Impossible to do such an affectation to an integer"))
      | TPTR t1, Some t2 when t1 <> t2 -> raise (Type_Error (l, "Expression on the right of affectation don't have the right type"))
      | _ -> () 
    end



let check_call tast_list s l =
  let rec compare_types tast_l type_l = 
    match tast_l, type_l with 
    |[], [] -> ()
    |[], _ |_, [] -> 
      raise (Type_Error (l,"The function wasn't called with the right number of arguments"))
    | t_expr::q1, typ::q2 -> 
      let (t_opt,_) = t_expr in
      begin
        match t_opt with 
        |Some (t) when t = typ -> compare_types q1 q2
        | _ -> raise (Type_Error (l, "The function was called with wrong types of arguments"))
      end;
  in
  let type_list = get_args_types s l in
    compare_types tast_list type_list

  


let check_mon_op mon_op taste l = 
  let (t_opt, _) = taste in
  match mon_op with
    | M_MINUS  -> 
      begin 
        match t_opt with 
        | Some(TINT) -> ()
        | Some(TPTR _ ) -> raise (Type_Error (l, "Impossible to take the oposite of a pointer"))
        | None -> raise (Type_Error (l, "Impossible to take the oposite of None type"))
      end
    | M_NOT -> 
      begin 
        match t_opt with 
        | Some(TINT) -> ()
        | Some(TPTR _ ) -> raise (Type_Error (l, "Impossible to take the negation of a pointer"))
        | None -> raise (Type_Error (l, "Impossible to take the negation of None type"))
      end
    | M_POST_INC | M_PRE_INC ->
      begin 
        match t_opt with 
        | Some(TINT) | Some (TPTR _) -> ()
        | None -> raise (Type_Error (l, "Impossible to increment None type"))
      end 
    | M_POST_DEC | M_PRE_DEC -> 
      begin 
        match t_opt with 
        | Some(TINT) | Some (TPTR _)-> ()
        | None  -> raise (Type_Error (l, "Impossible to decrement None type"))
      end
    | M_DEREF ->
      begin
        match t_opt with
        | Some (TPTR _) -> ()
        | Some (TINT) -> raise (Type_Error (l,"Impossible to dereferentiate an integer"))
        | None -> raise (Type_Error (l, "Impossible to dereferentiate a None type")) 
      end
    | M_ADDR  ->  
      begin
        match t_opt with
          | Some (TINT) | Some (TPTR _) -> ()
          | None -> raise (Type_Error (l,"Impossible to get the adress of a None type"))
      end


let check_bin_op bin_op tast1 tast2 l =  
  let (t_opt1, _) = tast1 in
  let (t_opt2, _) = tast2 in
  match bin_op with
    | S_MUL  -> 
      if (t_opt1 <> Some (TINT) || t_opt2 <> Some (TINT)) then raise(Type_Error (l, "Impossible to multiply a non integer expression"))
    | S_DIV ->
      if (t_opt1 <> Some (TINT) || t_opt2 <> Some (TINT)) then raise(Type_Error (l, "Impossible to divide (by) a non integer expression"))
    | S_MOD ->
      if (t_opt1 <> Some (TINT) || t_opt2 <> Some (TINT)) then raise(Type_Error (l, "Impossible to take mod of a non integer expression"))
    | S_ADD -> 
      begin
        match t_opt1, t_opt2 with
        | Some (TINT), Some (TINT) | Some (TPTR _), Some (TINT)-> ()
        | Some (TPTR _), Some (TPTR _) -> raise (Type_Error (l, "Impossible to add pointers with one another"))
        | Some (TINT), Some (TPTR _) -> raise (Type_Error (l, "Addition between poitner and integer is in the wrong order : change with ptr + int"))
        | None, _ | _, None -> raise (Type_Error (l, "Impossible to make addition with None type"))
      end 
    | S_SUB ->
      begin
        match t_opt1, t_opt2 with
        | Some (TINT), Some (TINT) | Some (TPTR _), Some (TINT)  -> ()
        | Some (TPTR t1), Some (TPTR t2) when t1=t2 -> ()
        | Some (TPTR _), Some (TPTR _) -> raise (Type_Error (l, "Impossible to substract pointers with different types"))
        | Some (TINT), Some (TPTR _) -> raise (Type_Error (l, "Substraction between poitner and integer is in the wrong order : change with ptr + int"))
        | None, _ | _, None -> raise (Type_Error (l, "Impossible make substraction with None type"))
      end 

let check_cmp cmp_op tast1 tast2 l = 
  let (t_opt1, _) = tast1 in 
  let (t_opt2, _) = tast2 in
  if (t_opt1 <> t_opt2) then 
    begin
      match cmp_op with 
      | C_LT -> raise (Type_Error (l, "Types are not the same on both sides of '<'"))
      | C_LE -> raise (Type_Error (l, "Types are not the same on both sides of '<='"))
      | C_EQ -> raise (Type_Error (l, "Types are not the same on both sides of '=='"))
    end

let check_eif tast1 tast2 tast3 l = 
  let (t_opt1, _) = tast1 in
  let (t_opt2, _) = tast2 in
  let (t_opt3, _) = tast3 in 
  if (t_opt2 = t_opt3 ) then  
    begin
    match t_opt1 with
      | Some TINT -> ()
      | _ -> raise (Type_Error (l, "Condition of 'if' is not typed int"))
    end 
  else raise (Type_Error (l, "Blocs of after 'if' and 'else' don't have the same type "))





(* GENERATE TYP_EXPR FOR TAST *)
let rec check_loc_expr le =
  let (l, exp) = le in
  check_expr exp l 

and check_expr exp l = match exp with 
  | Cast.VAR s -> (Some (get_type s l), Tast.VAR s) 
  | Cast.CST n -> (Some TINT, Tast.CST n)
  | Cast.STRING s -> (Some (TPTR TINT), Tast.STRING s)
  | Cast.SET_VAR (s, le) -> 
    let taste = check_loc_expr le in
    check_set_var s taste l;
    let (t_opt, _) = taste in
    t_opt, Tast.SET_VAR (s, taste)
  | Cast.SET_VAL (s, le) -> 
    let taste = check_loc_expr le in
    let (t_opt, _) = taste in 
    check_set_val s taste l;
    t_opt, Tast.SET_VAL (s, taste)
  | Cast.CALL (s, le_list) -> 
    let tast_list = List.map check_loc_expr le_list in
    check_call tast_list s l;
    (Some (get_type s l)), Tast.CALL (s, tast_list)
  | Cast.OP1 (m_op, le) -> 
    let taste = check_loc_expr le in
    let (t_opt, _) = taste in
    check_mon_op m_op taste l;
    t_opt, Tast.OP1 (m_op, taste)
  | Cast.OP2 (b_op, le1, le2) ->
    let taste1 = check_loc_expr le1 in
    let taste2 = check_loc_expr le2 in 
    let (t_opt, _) = taste2 in 
    check_bin_op b_op taste1 taste2 l;
    t_opt, Tast.OP2 (b_op, taste1, taste2)
  | Cast.CMP (cmp_op, le1, le2) ->
    let taste1 = check_loc_expr le1 in
    let taste2 = check_loc_expr le2 in
    check_cmp cmp_op taste1 taste2 l;
    Some (TINT), Tast.CMP (cmp_op, taste1, taste2)
  | Cast.EIF (le1, le2, le3) -> 
    let taste1 = check_loc_expr le1 in
    let taste2 = check_loc_expr le2 in
    let taste3 = check_loc_expr le3 in
    let (t_opt, _) = taste2 in
    check_eif taste1 taste2 taste3 l;
    t_opt, Tast.EIF (taste1, taste2, taste3)
  | Cast.ESEQ le_list ->
    let taste_list = List.map check_loc_expr le_list in
    let t_opt = 
      begin
        try let (typ_opt,_ ) = List.hd taste_list in typ_opt
        with _ -> None 
      end
    in 
    t_opt, Tast.ESEQ (taste_list)

(* CHECKING FUNCTIONS FOR CODES *)

let check_if taste tastc1 tastc2 l =  
  let (t_opte, _) = taste in
  match t_opte with
    | Some TINT -> ()
    | _ -> raise (Type_Error (l, "Condition of 'if' is not typed int"))

  

let check_while taste tastc l  =  
  let (t_opt, _ ) = taste in
  match t_opt with
  | Some TINT -> ()
  | _ -> raise (Type_Error (l,"Condition of 'while' is not typed int"))

let check_return le_opt = 
  match le_opt with
  | None -> None 
  | Some le -> 
    let tast = check_loc_expr le in
    Some tast


let check_codelist_return code_list l = 
  let rec check_return_aux a_list typ_opt = match a_list with
    | [] -> typ_opt
    | (None, _)::q -> check_return_aux q typ_opt
    | (Some t, _)::q when typ_opt = None -> check_return_aux q (Some t)
    | (typ_opt2, _)::q when typ_opt <> typ_opt2 -> raise (Type_Error (l, "The function has two returns with types differents"))
    | _::q -> check_return_aux q typ_opt;
  in
    check_return_aux code_list None

let check_return_if tastc1 tastc2 loc = 
  let (t_opt1, _) = tastc1 in
  let (t_opt2, _) = tastc2 in 
  match t_opt1, t_opt2 with
  | None, _ |_, None -> None
  | Some t1, Some t2 when t1<>t2 ->
     raise (Type_Error (loc, "Returns in 'if' and return in 'else' bloc don't have the same type"))
  | Some t1, _ -> Some t1

(* GENERATE TYP_CODE FOR TAST*)
let rec check_var_declaration v = match v with
 | Cast.CDECL (pos, name, typ) -> 
    push (var_declaration_loc_create v false); 
    Tast.CDECL (name, typ)
 | CFUN (pos, name, args, typ, l_code) -> raise (Env.Declaration_Error (pos, "Impossible to declare a local function"))

and check_loc_code l_code = 
  let (_,c) = l_code in
  check_code c
    
and check_code cod = match cod with  
    | Cast.CBLOCK (decs, loc_codes) -> 
      let dec_list = List.map check_var_declaration decs in
      let code_list = List.map check_loc_code loc_codes in       
      begin
        try 
        let l = 
            match List.hd decs with
              | Cast.CDECL (pos,_,_) -> pos
              | Cast.CFUN (pos,_,_,_,_) -> pos
          in 
          let t_opt = check_codelist_return code_list l in 
          pop_mult (List.length dec_list) l ; 
          t_opt, Tast.CBLOCK (dec_list, code_list)
        with 
        | Type_Error (l,s) -> raise(Type_Error (l,s))
        | Failure (hd) -> None, Tast.CBLOCK(dec_list, code_list)  
      end 
    | Cast.CEXPR le -> 
      let taste = check_loc_expr le in 
      None, Tast.CEXPR (taste)
    | Cast.CIF (le, lc1, lc2) -> 
      let taste = check_loc_expr le in 
      let tastc1 = check_loc_code lc1 in
      let tastc2 = check_loc_code lc2 in
      let (l,_) = le in
      let t_opt = check_return_if tastc1 tastc2 l in 
      check_if taste tastc1 tastc2 l;
      t_opt, Tast.CIF (taste, tastc1, tastc2)
    | Cast.CWHILE (le, lc) ->
      let taste = check_loc_expr le in
      let tastc = check_loc_code lc in
      let (l,_) = le in 
      let (t_opt, _) = tastc in
      check_while taste tastc l;
      t_opt, Tast.CWHILE (taste, tastc)
    | Cast.CRETURN le_opt -> 
      let taste_opt = check_return le_opt in 
      let t_opt = 
      begin
         match taste_opt with 
        | None -> Some (TINT)  (* return; <=> return 0; *)
        | Some (taste) -> let (typ_opt, _) = taste in typ_opt 
      end
      in
      t_opt, Tast.CRETURN (taste_opt)


let check_var_declaration_init v = match v with
| Cast.CDECL (pos, name, typ) -> 
   push (var_declaration_loc_create v true);
   Tast.CDECL (name, typ)
| Cast.CFUN (pos, name, args, typ, l_code) -> 
   let taste_list = List.map check_var_declaration args in
   let tastc = check_loc_code l_code in
   let (t_opt, _) = tastc in 
   pop_mult (List.length args) pos ;
   push (var_declaration_loc_create v false);
   begin
    match t_opt with
    | None -> raise (Type_Error (pos, "The function don't have a return in every branch"))
    | Some t when t <> typ -> raise (Type_Error (pos, "Return type does not match with function type"))
    | _ -> Tast.CFUN (name,taste_list, typ, tastc)
   end

let check_file var_dec_l = List.map check_var_declaration_init var_dec_l