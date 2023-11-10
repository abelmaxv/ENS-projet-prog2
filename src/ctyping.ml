open Cast


let checkfile file = match file with 
| CDECL (loc, name, typ) -> checkvar 
| CFUN (location, name, declist, typ, code) -> 


