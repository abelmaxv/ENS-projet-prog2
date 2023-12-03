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
  val get_pos : string -> int (*Returns the position by name*)
  val get_loc : string -> bool (* Returns if a variable if local or not*)
end

module Symbol_tab:Symbol_table_t = 
struct
  type item = {name : string; loc : bool; pos : int} (*name, local or not, position (global) /relative position (local)*)

  type symbol_table_t = item Stack.t

  let symbol_table = Stack.create()

  let add (i:item) = Stack.push i symbol_table 

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
end


(*__________________________ LC-3 GENERATION __________________________*)
open Tast
open Symbol_tab

(* Concatenates elements of a list of strings *)
let rec cat_list string_l = 
  match string_l with
  | [] -> ""
  | s::q -> s^cat_list q

let label_counter = ref 0  

let label_generator () = 
  incr label_counter;
  "label"^string_of_int(!label_counter)



let rec compile_var_declaration typ_vd = 
  match typ_vd with
  | Tast.CDECL (name, _) -> name ^ " .FILL 0 \n"
  | Tast.CFUN (name, typ_vd, typ, typ_code) -> name^": ADD R6, R6, #-1 \n" ^ compile_code typ_code
(* TO DO *)
and compile_code typ_code = 
    let (_, code) = typ_code in
    match code with 
    | Tast.CBLOCK (var_dec_l, t_code_l) -> ""
    | Tast.CEXPR typ_expr -> ""
    | Tast.CIF (typ_expr, typ_code1, typ_code2) -> ""
    | Tast.CWHILE (typ_expr, typ_code)->""
    | Tast.CRETURN (typ_expr_opt) -> ""

let compile_file f = ".ORIG x3000 \nBR main \n" ^ cat_list (List.map compile_var_declaration f) (*^ static variable *)