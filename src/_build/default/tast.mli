open Cast

type typ = Tint | Tptr of typ

type typed_expr = expr*typ
type typed_var_declaration = var_declaration*typ