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
  val get_pos : string -> int (* Returns the position by name*)
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

  let pop () = let _ = Stack.pop symbol_table in  ()

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
let string_location = Queue.create()  (* Stores the position of strings in the static memory, just after the code *)

(* Concatenates elements of a list of strings *)
let rec cat_list string_l = 
  match string_l with
  | [] -> ""
  | s::q -> s^cat_list q

(* Returns the number of lines in the string s*)
let count_ligns s = 
  let n = String.length s in
  let counter = ref 0 in
  for i = 0 to n-1 do
    if s.[i] = '\n' then 
      incr counter 
  done;
  !counter

    let label_counter = ref 0 

(* Generates a fresh label *)
let label_generator () = incr label_counter; "label" ^ string_of_int(!label_counter)


(*__________________________ LC-3 GENERATION __________________________*)
open Tast
open Symbol_tab

(* First Pass : location of strings in the memory *)
let rec string_expr typ_expr = 
  let (_, expr) = typ_expr in
  match expr with
  | Tast.VAR _ | Tast.CST _ -> ""
  | Tast.STRING s ->  
    (* Creates the string memory location and increases global_counter for other global variable*)
    Queue.add !global_counter string_location;
    global_counter := !global_counter + String.length s + 1;
    ".STRINGZ " ^ s ^ " \n"

  | Tast.SET_VAR (_, typ_expr) | Tast.SET_VAL (_, typ_expr) | Tast.OP1 (_, typ_expr) -> 
    string_expr typ_expr
  | Tast.OP2 (_, typ_expr1, typ_expr2) | Tast.CMP (_, typ_expr1, typ_expr2) ->
    let str_expr1 = string_expr typ_expr1 in
    let str_expr2 = string_expr typ_expr2 in 
    str_expr1 ^ str_expr2 
  | Tast.EIF (typ_expr1, typ_expr2, typ_expr3) -> 
    let str_expr1 = string_expr typ_expr1 in
    let str_expr2 = string_expr typ_expr2 in 
    let str_expr3 = string_expr typ_expr3 in 
    str_expr1 ^ str_expr2 ^ str_expr3
  | Tast.ESEQ typ_expr_l | Tast.CALL (_, typ_expr_l) ->
    cat_list (List.map string_expr typ_expr_l)

and string_var_declaration typ_vd = 
  match typ_vd with
  | Tast.CFUN (name, typ_vd, typ, typ_code) -> 
    string_code typ_code 
  | _ -> ""

and  string_code  typ_code = 
  let (_,code) = typ_code in
  match code with
  | Tast.CBLOCK (var_dec_l, t_code_l) -> 
    let str_var = cat_list (List.map string_var_declaration var_dec_l) in
    let str_code = cat_list (List.map string_code t_code_l) in 
    str_var ^ str_code
  | Tast.CEXPR typ_expr ->
    string_expr typ_expr
  | Tast.CIF (typ_expr, typ_code1, typ_code2) -> 
    let str_expr = string_expr typ_expr in
    let str_code1 = string_code typ_code1 in
    let str_code2 = string_code typ_code2 in
    str_expr ^ str_code1 ^ str_code2
  | Tast.CWHILE (typ_expr, typ_code)->
    let str_expr = string_expr typ_expr in
    let str_code = string_code typ_code in
    str_expr ^ str_code
  | Tast.CRETURN (Some typ_expr) -> 
    string_expr typ_expr  
  | _ -> ""


(* Second pass : compilation *)


let compile_mon_op mon_op = 
  match mon_op with
  | Cast.M_MINUS -> 
    "NOT R0, R0 \nADD R0, R0, #1 ; R0 <- -R0 \n"
  | Cast.M_NOT -> 
    "NOT R0 R0 ; R0 <- ~R0\n"
  | Cast.M_POST_INC -> 
    "LDR R1, R0, #0 ; R1 <- M[R0] \nADD R1, R1, #1 ; R1 <- R1 + 1 \nSTR R1, R0, #0 ; M[R0] <- R1 \nADD R0, R1, #-1 ; R0 <- R1-1 (x++) \n"
  | Cast.M_POST_DEC -> 
    "LDR R1, R0, #0 ; R1 <- M[R0] \nADD R1, R1, #-1 ; R1 <- R1 - 1 \nSTR R1, R0, #0 ; M[R0] <- R1 \nADD R0, R1, #1 ; R0 <- R1+1 (x--) \n"
  | Cast.M_PRE_INC -> 
    "LDR R1, R0, #0 ; R1 <- M[R0] \nADD R1, R1, #1 ; R1 <- R1+1 \nSTR R1, R0, #0 ; M[R0]<-R1 \nADD R0, R1, #0 ; R0 <- R1 (++x) \n"
  | Cast.M_PRE_DEC -> 
    "LDR R1, R0, #0 ; R1 <- M[R0] \nADD R1, R1, #-1 ; R1 <- R1+1 \nSTR R1, R0, #0 ; M[R0]<-R1 \nADD R0, R1, #0 ; R0 <- R1 (--x) \n"
  | Cast.M_DEREF -> 
    "LDR R0 R0 #0 \nLDR R0,R0, #0 ; R0 <- M[M[R0]] (*x) \n"
  | Cast.M_ADDR ->
    (*with the compile_typ_expr R0 contains the address of the variable*) 
    "" 



let compile_bin_op bin_op = 
  match bin_op with
  | S_MUL -> 
    let loop_label = label_generator() in
    let end_label = label_generator() in
    (* Algorithm we've seen in class *)
    (* R2 <- R1*R0 then R0 <- R2 *)
    "AND R2, R2, #0 \nAND R1, R1, R1 \nBRz end_" ^ end_label ^ "\nBRp loop_" ^ loop_label ^ "\nNOT R0, R0 \nADD R0, R0, #1 \nNOT R1, R1 \nADD R1, R1, #1 \nloop_" ^ loop_label ^ " ADD R2, R2, R0 \nADD R1, R1, #-1 \nBRnp loop_" ^ loop_label ^ "\nend_" ^ end_label ^ "  ADD R0, R2, #0 \n"
  | S_DIV ->
    let error_label = label_generator() in
    let zero_neg_label = label_generator() in
    let test_one_label = label_generator() in
    let loop_label = label_generator() in
    let end_label = label_generator() in 
    (* R3 = 0 iif R0 and R1 have the same sign *)
    (*R2 <- R1/R0 then R0 <- R2  *)
    "AND R3, R3, #0 ; R3 <- 0 \nADD R2, R3, #-1 ; R2 <- -1 \nADD R0, R0, #0 ; Tests the sign of R0\nBRz error_" ^ error_label ^" \nBRn zero_neg_" ^ zero_neg_label ^ " \nNOT R0, R0 \nADD R0, R0, #1 ; if R0>0 then R0 <- -R0 \nBR test_one_" ^ test_one_label ^ " \nzero_neg_" ^ zero_neg_label ^ " ADD R3, R3, #1 \ntest_one_" ^ test_one_label ^ " ADD R1, R1, #0 ; Tests the sign of R1 \nBRzp loop_" ^ loop_label ^ " \nNOT R3, R3 \nADD R3, R3, #2 ; R3 <- 1-R3 \nNOT R1,R1 \nADD R1, R1, #1 ; R1 <- -R1 \nloop_" ^ loop_label ^ " ADD R2, R2, #1 ; R2 <- R2+1 \nADD R1, R1, R0 ; R1<-R1+R0 \nBRp loop_" ^ loop_label ^ " \nADD R3, R3, #-1 \nBRnp end_" ^ end_label ^ " ; Tests if R3 = 1  \nNOT R2, R2 \nADD R2, R2, #1 ; R2  <- -R2 \nBR end_" ^ end_label ^ " \nerror_" ^ error_label ^ " ; TO COMPLETE \nend_" ^ end_label ^ " ADD R0, R2, #0 ; R0 <- R2 \n"
    (* TO DO : DIVISION BY ZERO TO HANDLE *)
  | S_MOD -> 
    let error_label = label_generator() in
    let test_one_label = label_generator() in
    let loop_label = label_generator() in
    let end_label = label_generator() in 
    (* R0 <- R1 mod R0 doing R2 <- R1/R0 then R0 <- R1-R2*)
    "AND R3,R3,#0 ; R3 <- 0 \nADD R0, R0, #0 ; Tests the sign of R0 \nBRz error_" ^ error_label ^ "\nBRn test_one_" ^ test_one_label ^ "\nNOT R0, R0 \nADD R0, R0, #1 ; if R0>0 then R0 <- -R0 \ntest_one_" ^ test_one_label ^ " ADD R1, R1, #0 ; Tests the sign of R1 \nBRzp loop_" ^ loop_label ^ " \nNOT R1, R1 \nADD R1, R1, #1 ; if R1<0 then R1 <- -R1\nADD R3, R3, #1 ; Mark R1 changed sign \nloop_" ^ loop_label ^ " ADD R1, R1, R0 ; R1 <- R1 + R0 \nBRzp loop_" ^ loop_label ^ "\nNOT R0, R0 \nADD R0, R0, #1 ; R0 <- -R0 \nADD R0, R1, R0 ; R0 <- R1 + R0 \nADD R3, R3, #-1 ; Tests if R3 = 1 \nBRnp end_" ^ end_label ^" \nNOT R0, R0 \nADD R0, R0, #1 ; R0 <- -R0\nBR end_" ^ end_label ^ " \nerror_" ^ error_label ^ " ; TO COMPLETE \nend_" ^ end_label ^ " "
      (* TO DO : DIVISION BY ZERO TO HANDLE *)
  | S_ADD ->
    (*R0 <- R0+R1*)
    "ADD R0, R0, R1 ; R0 <- R0 + R1 \n"
  | S_SUB -> 
    (* R0 <- R1-R0 *)
    "NOT R0, R0 \nADD R0, R0, #1 \nADD R0, R0, R1 \n"



let compile_cmp_op cmp_op = 
  match cmp_op with 
  | C_LT -> 
    let neg_label = label_generator() in
    let end_label = label_generator() in
    "NOT R0, R0 \nADD R0, R0, #1 \nADD R1, R1, R0 \nBRn neg_" ^ neg_label ^ " \nAND R0, R0, #0 \nBR end_" ^ end_label ^ " \nneg_" ^ neg_label ^ " AND R0, R0, #0 \nADD R0, R0, #1 \nend_" ^ end_label ^ " "
  | C_LE -> 
    let neg_label = label_generator() in
    let end_label = label_generator() in
    "NOT R0, R0 \nADD R0, R0, #1 \nADD R1, R1, R0 \nBRzn neg_" ^ neg_label ^ " \nAND R0, R0, #0 \nBR end_" ^ end_label ^ " \nneg_" ^ neg_label ^ " AND R0, R0, #0 \nADD R0, R0, #1 \nend_" ^ end_label ^ " "
  | C_EQ -> 
    let eq_label = label_generator() in
    let end_label = label_generator() in
    "NOT R0, R0 \nADD R0, R0, #1 \nADD R1, R1, R0 \nBRz eq_" ^ eq_label ^ " \nAND R0, R0, #0 \nBR end_" ^ end_label ^ " \neq_" ^ eq_label ^ "  AND R0, R0, #0 \nADD R0, R0, #1 \nend_" ^ end_label ^ " "



let rec compile_typ_expr addr typ_expr = 
  let (_, expr) = typ_expr in
  compile_expr addr expr

and compile_expr addr expr = 
  (* If addr = true : Var name -> address of name
              Else : Var name -> content of name *)
  match expr with
  | Tast.VAR name ->
    let cte_label = label_generator() in 
    if addr then 
      (* RO <- addr of name *)
      begin 
        if (is_loc name) then
          "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #-" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R0, R2, R5 ; R0 <- R5 - offset (addr of local) \n"
        else
          "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R0, R4, R2 ; R0 <- R4 + offset (addr of gloabl) \n"
      end 
    else  
      (* R0 <- value of name *)
      begin
        if (is_loc name) then
          "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #-" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R5 \nLDR R0, R2, #0 ; R0 <- M[R5-offset] (value of local)  \n"
        else
          "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R4 \nLDR R0, R2, #0 ; R0 <- M[R4+offset] (value of gloabl) \n"
      end
  | Tast.CST n -> 
    let cte_label = label_generator() in
    "LD R0 cte_" ^ cte_label ^ " \nBR cte_ignore_" ^ cte_label ^ "\ncte_" ^ cte_label ^ " .FILL #" ^ string_of_int(n) ^ " ; R0 <-  " ^ string_of_int n ^ " \ncte_ignore_" ^ cte_label ^ " "
  | Tast.STRING s ->
    let cte_label = label_generator() in 
    let n = Queue.take string_location in 
    "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_" ^ cte_label ^ " .FILL #" ^ string_of_int n ^ "\nignore_cte_" ^ cte_label ^ " ADD R0, R2, R4 ; R0 <- adress of a string \n"
  | Tast.SET_VAR (name, typ_expr) -> 
    let expr_asm = compile_typ_expr false typ_expr in
    let cte_label = label_generator() in
    expr_asm ^
    if (is_loc name) then
      "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #-" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R5 \nSTR R0, R2, #0 ; M[R5 - offset] <- R0 (x = e with x local) \n"
    else 
      "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R4 \nSTR R0, R2, #0 ; M[R4 + offset]<- R0 (x = e with x global \n"
  | Tast.SET_VAL (name, typ_expr) ->
    let expr_asm = compile_typ_expr false typ_expr in
    let cte_label = label_generator() in
    expr_asm ^
    if (is_loc name) then
      "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #-" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R5 \nLDR R2, R2, #0  ; R2 <- M[R5 - offset] \nSTR R0, R2, #0 ; M[R2] <- R0 (*x = e with x local) \n"
    else 
      "LD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_"^ cte_label ^ " .FILL #" ^ string_of_int(get_pos name) ^ "\nignore_cte_" ^ cte_label ^ " ADD R2, R2, R4 \nLDR R2, R2, #0  ; R2 <- M[R4 + offset] \nSTR R0, R2, #0 ; M[R2] <- R0 (*x = e with x global) \n"
  | Tast.CALL (name, typ_expr_l) -> 
    "; CALLING FUNCTIONS IS YET TO IMPLEMENT \n" (*TO DO*)
  | Tast.OP1 (mon_op, typ_expr) -> 
    let expr_asm = 
      begin
        match mon_op with 
        | M_MINUS | M_NOT -> compile_typ_expr false typ_expr (* For these operation we need the value of a variable *)
        |_ -> compile_typ_expr true typ_expr (* The other we need the  adress of a variable *)
      end 
    in
    let mon_op_asm = compile_mon_op mon_op in
    expr_asm ^ mon_op_asm
  | Tast.OP2 (bin_op, typ_expr1, typ_expr2) -> 
    let expr_asm1 = compile_typ_expr false typ_expr1 in
    let expr_asm2 = compile_typ_expr  false typ_expr2 in
    let bin_op_asm = compile_bin_op bin_op in
    (*Compiles e1 in R0, puts in stack, compiles e2 in R0, pops stack in R1, then does operation on R0 and R1*)
    expr_asm1 ^ "STR R0, R6, #0\nADD R6, R6, #-1 ; puts the evaluation of an expression on the stack\n" ^ expr_asm2 ^ "ADD R6, R6, #1\nLDR R1, R6, #0  ; pops stack to do a binary operation\n" ^ bin_op_asm
  | Tast.CMP (cmp_op, typ_expr1, typ_expr2) -> 
    let expr_asm1 = compile_typ_expr false typ_expr1 in
    let expr_asm2 = compile_typ_expr false typ_expr2 in
    let cmp_asm = compile_cmp_op cmp_op in
    (*Same as OP2*)
    expr_asm1 ^ "STR R0, R6, #0\nADD R6, R6, #-1 ; puts the evaluation of an expression on the stack\n" ^ expr_asm2 ^ "ADD R6, R6, #1\nLDR R1, R6, #0  ; pops stack to do a comparison \n" ^ cmp_asm
  | Tast.EIF (typ_expr1, typ_expr2, typ_expr3) -> 
    let expr_asm1 = compile_typ_expr false typ_expr1 in
    let expr_asm2 = compile_typ_expr false typ_expr2 in
    let expr_asm3 = compile_typ_expr false typ_expr3 in
    let label_false = label_generator() in
    expr_asm1 ^ "BRz blockFalse_" ^ label_false ^ " " ^ expr_asm2 ^ "blockFalse_" ^ label_false ^ " " ^ expr_asm3
  | Tast.ESEQ typ_expr_l -> 
    cat_list (List.map (compile_typ_expr false) typ_expr_l )


  

let rec compile_var_declaration typ_vd = 
  match typ_vd with
  | Tast.CDECL (name, _) -> (* declaration of a local variable *)
    add (create_item name true (!local_counter)); 
    incr local_counter;
    "ADD R6, R6, #-1 ; Puts the local variable " ^ name ^ " on top of the stack \n"
  | Tast.CFUN (name, typ_vd, typ, typ_code) -> 
    failwith "Declaration of a local function"

and compile_code typ_code = 
    let (_, code) = typ_code in
    match code with 
    | Tast.CBLOCK (var_dec_l, t_code_l) -> 
      let s1 = cat_list (List.map compile_var_declaration var_dec_l) in
      let s2 = cat_list (List.map compile_code t_code_l) in
      s1^s2
    | Tast.CEXPR typ_expr -> 
      compile_typ_expr true typ_expr
    | Tast.CIF (typ_expr, typ_code1, typ_code2) -> 
      let condition_asm = compile_typ_expr false typ_expr in
      let code_asm1 = compile_code typ_code1 in
      let code_asm2 = compile_code typ_code2 in
      let label_true = label_generator() in
      let cte_label = label_generator() in
      let cte_label2 = label_generator() in
      let nb_lines1 = count_ligns code_asm1 in
      let nb_lines2 = count_ligns code_asm2 in
      condition_asm ^ "ADD R0, R0, #0 ; Tests if the IF condition is true \nBRnp true_" ^ label_true ^ "\nLEA R0, #11 \nLD R2 cte_" ^ cte_label ^ "\nBR ignore_cte_" ^ cte_label ^ "\ncte_" ^ cte_label ^ " .FILL #" ^ string_of_int nb_lines1 ^ " \nignore_cte_" ^ cte_label ^ " ADD R0, R0, R2\nJMP R0 \n" ^ "true_" ^ label_true ^ " " ^ code_asm1 ^ "\nLEA R0, #5 \nLD R2 cte_" ^ cte_label2 ^ "\nBR ignore_cte_" ^ cte_label2 ^ "\ncte_" ^ cte_label2 ^ " .FILL #" ^ string_of_int nb_lines2 ^ " \nignore_cte_" ^ cte_label2 ^ " ADD R0, R0, R2\nJMP R0 \n" ^ code_asm2
    | Tast.CWHILE (typ_expr, typ_code)->
      let condition_asm = compile_typ_expr false typ_expr in
      let code_asm = compile_code typ_code in
      let label_condition = label_generator() in
      let label_end = label_generator() in
      "cond_" ^ label_condition ^ " " ^ condition_asm ^ "ADD R0, R0, #0 ; Tests if the condition is true \n" ^ "BRz end_" ^ label_end ^ " ; If not go to the end of the while block \n" ^ code_asm ^ "BR cond_" ^ label_condition ^ "\nend_" ^ label_end 
    | Tast.CRETURN (Some typ_expr) -> (* TO DO *)
      "; RERTURN IS YET TO IMPLEMENT \n" 
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
    local_counter := 0;
    (* TO DO : IMPLEMENT FUNCTION *)
    name ^ " " ^ var_asm ^ code_asm

let compile_file f = 
  let string_mem = cat_list (List.map string_var_declaration f) in 
  let code = cat_list (List.map compile_var_declaration_init f)  in
  let l = count_ligns code in
  let header = ".ORIG x3000 \nLD R6 init_stack \nBR ignore_init_stack \ninit_stack .FILL #65503\nignore_init_stack ADD R5, R6, #0 \nLD R4 init_static \nBR ignore_init_static \ninit_static .FILL #" ^ string_of_int (l + 7 + 12288) ^ "\nignore_init_static BR main \n" in 
  let footer =  ".END \n" in
  header ^ code ^ string_mem ^ footer