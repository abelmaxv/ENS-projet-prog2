open Cast

let (env : var_declaration list) = []

let check_vardec d = match d with 
 |CDECL 

let rec check_vardec_list l = match l with
  |[] -> ()
  |t::q -> check_vardec t;
          check_vardec_list d
    

let checkfile file = match file with 
  | CBLOCK (var_decs, loc_cods) -> check_vardec_list

