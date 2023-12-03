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


(*_________________________ SYMBOL TABLE DEFINITION _________________________ *)
open Cast


module type Symbol_table_t =
sig
  type item
  type symbol_table_t
  val symbol_table : symbol_table_t
  val add : item -> unit
  val get_pos : string -> int (*Returns the position by name*)
end

module Symbol_tab:Symbol_table_t = 
struct
  type item = {name : string; loc : bool; pos : int} (*name, local or not, position/relative position*)

  type symbol_table_t = item Stack.t

  let symbol_table = Stack.create()

  let add i = Stack.push i symbol_table 

  let rec get_pos name = 
    if (Stack.is_empty symbol_table) then
      failwith "The symbol table is empty"
    else
      begin
        let (s, l, p1) = Stack.pop symbol_table in
        if name = s then
          p1
        else
          begin
            let p2 = get_pos name in
            Stack.push {name = s; loc = l; pos = p1};
            p2
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


(* TO DO *)
let rec compile_typ_expr 


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