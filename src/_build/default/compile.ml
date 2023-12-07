(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 3
*
* ABEL VERLEY
*__________________________________________________________________
*)
(* Ce code permet, a partir un l'ast typé fournit par la fonction check_file du module ctyping, 
   une traducction d'un programme C en LC-3 *)


(*_________________________ SYMBOL TABLE DEFINITION (Stack) _________________________ *)
open Cast


module type Symbol_table_t =
sig
  type item
  type symbol_table_t
  val symbol_table : symbol_table_t
  val add : item -> unit
  val pop : unit -> unit
  val get_pos : string -> int (*Returns the position by name*)
  val get_loc : string -> bool (* Returns if a variable if local or not*)
  val create_item : string -> bool -> int -> item
  val pop_multiple : int -> unit
end

module Symbol_tab:Symbol_table_t = 
struct
  type item = {name : string; loc : bool; pos : int} (*name, local or not, position (global) /relative position (local)*)

  type symbol_table_t = item Stack.t

  let symbol_table = Stack.create()

  let add i = Stack.push i symbol_table 

  let pop () = let _ = Stack.pop symbol_table in ()

  let rec get_pos name = 
    if Stack.is_empty symbol_table then
      failwith "The name was not found in the symbol table"
    else
      begin
        let i = Stack.pop symbol_table in
        if name = i.name then
          begin
            Stack.push i symbol_table;
            i.pos
          end
        else
          begin
            let p = get_pos name in
            Stack.push i symbol_table;
            p
          end
      end


  let rec get_loc name = 
    if Stack.is_empty symbol_table then
      failwith "The name was not found in the symbol table"
    else
      begin
        let i = Stack.pop symbol_table in
        if name = i.name then
          begin
            Stack.push i symbol_table;
            i.loc
          end
        else
          begin
            let l = get_loc name in
            Stack.push i symbol_table;
            l
          end
      end
    
    let create_item name loc pos = {name = name; loc = loc; pos = pos}

    let rec pop_multiple n = 
      match n with
      |0 -> ()
      |_ -> pop(); pop_multiple (n-1)
end










(*__________________________ LC-3 GENERATION __________________________*)
open Tast
open Symbol_tab

let local_counter = ref 0 (* Counts the number of vraiable declared in a block*)
let global_counter = ref 0  (* Counts the number of global variable declared *)


(* Concatenates elements of a list of strings *)
let rec cat_list string_l = 
  match string_l with
  | [] -> ""
  | s::q -> s^cat_list q

let rec compile_var_declaration typ_vd = 
  match typ_vd with
  | Tast.CDECL (name, _) -> (* declaration of a local variable *)
    add (create_item name true (!local_counter)); 
    incr local_counter;
    "ADD R6 R6 #-1 ;Puts the local variable " ^ name ^ " on top of the stack \n"
  | Tast.CFUN (name, typ_vd, typ, typ_code) -> failwith "Declaration of a local function"
(* TO DO *)
and compile_code typ_code = 
    let (_, code) = typ_code in
    match code with 
    | Tast.CBLOCK (var_dec_l, t_code_l) -> 
      cat_list (List.map compile_var_declaration var_dec_l) ^ cat_list (List.map compile_code t_code_l)
    | Tast.CEXPR typ_expr -> ""
    | Tast.CIF (typ_expr, typ_code1, typ_code2) -> ""
    | Tast.CWHILE (typ_expr, typ_code)->""
    | Tast.CRETURN (typ_expr_opt) -> ""


let compile_var_declaration_init typ_vd = 
  match typ_vd with
  | Tast.CDECL (name, _) -> (* declaration of a global variable *)
    add (create_item name false (!global_counter));
    incr global_counter;
    ""
  | Tast.CFUN (name, typ_vd, typ, typ_code) ->
    add (create_item name false (-1));
    local_counter := 0;
    let var_asm = cat_list (List.map compile_var_declaration typ_vd) in
    let code_asm = compile_code typ_code in
    pop_multiple (!local_counter);
    name ^ "\nADD R6 R6 #-1 \n; empile adresse retour R7 \nADD R6 R6 #-1 \nSTR R7 R6 #0 \n; empile base cadre R5 \nADD R6 R6 #-1 \nSTR R5 R6 #0 \n; nouvelle base \nADD R6 R6 #-2 \n" ^ var_asm ^ code_asm

let compile_file f = ".ORIG x3000 \nJSR main \n" ^ cat_list (List.map compile_var_declaration_init f) ^ ".END \n"