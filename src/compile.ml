(*
*__________________________________________________________________
* ENS L3 INFO : PROJET 2 PROGRAMMATION
* PARTIE 3
*
* ABEL VERLEY
*__________________________________________________________________
*)
(* Ce code permet, a partir un l'ast typé dournit par la fonction check_file du module ctyping, 
   une traducction d'un programme C en LC-3 *)


(*_________________________ SYMBOL TABLE DEFINITION _________________________ *)
open Cast


module type Symbol_table_t =
sig
  type item
  type symbol_table_t
  val symbol_table : symbol_table_t
  val add : item -> unit

end

module Symbol_table:Symbol_table_t = 
struct
  type item = 
    GLOBAL_VAR of string * ctyp * int (* Name, type, position *)
    | FUN of string * ctyp * int (* Name, type, position *)
    | LOCAL_VAR of string * int * ctyp * int (* Name, unique id, type, position*)
  
  type symbol_table_t = item Stack.t

  let symbol_table = Stack.create()
  
  let add item = Stack.push symbol_table item 

end


(*__________________________ LC-3 GENERATION __________________________*)
open Tast

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