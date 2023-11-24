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

and compile_code typ_code = 
    let (_, code) = typ_code in
    match code with 
    | Tast.CBLOCK (var_dec_l, t_code_l) -> ""
    | Tast.CEXPR typ_expr -> ""
    | Tast.CIF (typ_expr, typ_code1, typ_code2) -> ""
    | Tast.CWHILE (typ_expr, typ_code)->""
    | Tast.CRETURN (typ_expr_opt) -> ""

let compile_file f = ".ORIG x3000 \nBR main \n" ^ cat_list (List.map compile_var_declaration f)