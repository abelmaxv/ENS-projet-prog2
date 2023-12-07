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
  val is_loc : string -> bool (* Returns if a variable if local or not*)
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


  let rec is_loc name = 
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
            let l = is_loc name in
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






(*________________________ SOME USEFULL TOOLS _______________________*)

let local_counter = ref 0 (* Counts the number of vraiable declared in a block*)
let global_counter = ref 0  (* Counts the number of global variable declared *)


(* Concatenates elements of a list of strings *)
let rec cat_list string_l = 
  match string_l with
  | [] -> ""
  | s::q -> s^cat_list q

let label_counter = ref 0 

(* Generates a fresh label *)
let label_generator () = incr label_counter; "label" ^ string_of_int(!label_counter)


(*__________________________ LC-3 GENERATION __________________________*)
open Tast
open Symbol_tab

(*TO DO*)
let compile_cmp_op cmp_op = ""

(* TO DO *)
let compile_bin_op bin_op = ""

(*TO DO*)
let compile_mon_op mon_op = ""

let rec compile_typ_expr typ_expr = 
  let (_, expr) = typ_expr in
  compile_expr expr
  
and compile_expr expr =
  match expr with
  | Tast.VAR name -> 
    if (is_loc name) then
      "LDR R0 R6 #-" ^ string_of_int(get_pos name) ^ " \n"
    else
      "LDR R0 R4 #" ^ string_of_int(get_pos name) ^ " \n"
  | Tast.CST n -> 
    "AND R0 R0 #0 \n ADD R0 R0 #" ^string_of_int(n) ^ " \n"
  | Tast.STRING s -> 
    "String not handled yet \n" (*TO DO*)
  | Tast.SET_VAR (name, typ_expr) -> 
    let expr_asm = compile_typ_expr typ_expr in
    expr_asm ^
    if (is_loc name) then
      "STR R0 R6 #-" ^ string_of_int(get_pos name) ^ " \n"
    else 
      "STR R0 R4 #" ^ string_of_int(get_pos name) ^ " \n"
  | Tast.SET_VAL (name, typ_expr) -> 
    let expr_asm = compile_typ_expr typ_expr in
    expr_asm ^
    if (is_loc name) then
      "STR R0 R6 #-" ^ string_of_int(get_pos name) ^ " \n"
    else 
      "STR R0 R4 #" ^ string_of_int(get_pos name) ^ " \n"
  | Tast.CALL (name, typ_expr_l) -> 
    "Calling functions is yet to implement \n" (*TO DO*)
  | Tast.OP1 (mon_op, typ_expr) -> 
    let expr_asm = compile_typ_expr typ_expr in
    let mon_op_asm = compile_mon_op mon_op in
    expr_asm ^ mon_op_asm
  | Tast.OP2 (bin_op, typ_expr1, typ_expr2) -> 
    let expr_asm1 = compile_typ_expr typ_expr1 in
    let expr_asm2 = compile_typ_expr typ_expr2 in
    let bin_op_asm = compile_bin_op bin_op in
    (*Compile e1 in R0, puts in stack, compile e2 in R0, pop stack in R1, then operation on R0 and R1*)
    expr_asm1 ^ "ADD R6 R6 #-1 \nLDR R6 R0 #0\n" ^ expr_asm2 ^ "LDR R1 R6 #0 \nADD R6 R6 #1 \n" ^ bin_op_asm
  | Tast.CMP (cmp_op, typ_expr1, typ_expr2) -> 
    let expr_asm1 = compile_typ_expr typ_expr1 in
    let expr_asm2 = compile_typ_expr typ_expr2 in
    let cmp_asm = compile_cmp_op cmp_op in
    (*Same as OP2*)
    expr_asm1 ^ "ADD R6 R6 #-1 \nLDR R6 R0 #0\n" ^ expr_asm2 ^ "LDR R1 R6 #0 \nADD R6 R6 #1 \n" ^ cmp_asm
  | Tast.EIF (typ_expr1, typ_expr2, typ_expr3) -> 
    let expr_asm1 = compile_typ_expr typ_expr1 in
    let expr_asm2 = compile_typ_expr typ_expr2 in
    let expr_asm3 = compile_typ_expr typ_expr3 in
    let label_false = label_generator() in
    expr_asm1 ^ "BRz blockFalse_" ^ label_false ^ " \n" ^ expr_asm2 ^ "blockFalse_" ^ label_false ^ " \n" ^ expr_asm3
  | Tast.ESEQ typ_expr_l -> 
    cat_list (List.map compile_typ_expr typ_expr_l)


  

let rec compile_var_declaration typ_vd = 
  match typ_vd with
  | Tast.CDECL (name, _) -> (* declaration of a local variable *)
    add (create_item name true (!local_counter)); 
    incr local_counter;
    "ADD R6 R6 #-1 ; Puts the local variable " ^ name ^ " on top of the stack \n"
  | Tast.CFUN (name, typ_vd, typ, typ_code) -> failwith "Declaration of a local function"

and compile_code typ_code = 
    let (_, code) = typ_code in
    match code with 
    | Tast.CBLOCK (var_dec_l, t_code_l) -> 
      cat_list (List.map compile_var_declaration var_dec_l) ^ cat_list (List.map compile_code t_code_l)
    | Tast.CEXPR typ_expr -> 
      compile_typ_expr typ_expr
    | Tast.CIF (typ_expr, typ_code1, typ_code2) -> 
      let condition_asm = compile_typ_expr typ_expr in
      let code_asm1 = compile_code typ_code1 in
      let code_asm2 = compile_code typ_code2 in
      let label_false = label_generator() in
      let label_end = label_generator() in
      "; If block : evaluation of e in R0 \n" ^ condition_asm ^ "ADD RO R0 #0 ; Tests if the IF the contition is true \nBRz blockFalse_" ^ label_false ^ "; Go to block blockFalse if R0 = 0 \n" ^ code_asm1 ^ "BR endIf_" ^ label_end ^ "; Continues after the if instruction \n blockFalse_" ^ label_false ^ " ; Block if the condition is false \n" ^ code_asm2 ^ "endIf_" ^ label_end ^ " ; End of the if instruction \n"
    | Tast.CWHILE (typ_expr, typ_code)->
      let condition_asm = compile_typ_expr typ_expr in
      let code_asm = compile_code typ_code in
      let label_condition = label_generator() in
      let label_end = label_generator() in
      (* TO DO : CAN BE IMPROVED (cf cours) *)
      "; While block \n" ^ "cond_" ^ label_condition ^ " ; evaluation of the while condition in R0 \n" ^ condition_asm ^ "ADD R0 R0 #0 ; Tests if the condition is true \n" ^ "BRz end_" ^ label_end ^ " ; If not go to the end of the while block \n" ^ "; Code in the body of the while block : \n" ^ code_asm ^ "; End of the body of the while block \nBR cond_" ^ label_condition ^ "\nend_" ^ label_end ^ "\n End of the while block \n"
    | Tast.CRETURN (Some typ_expr) -> (* TO DO *)
      let expr_asm = compile_typ_expr typ_expr in
      "; Return block : evaluation of e in R0 \n" ^ expr_asm ^ "LDR R0 R5 #0 ; Puts the result in the return memory \n" (*..... TO CONTINUE *)
    | _ -> ""


(* TO DO : traitement des arguments de fonction different des variables locales *)
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