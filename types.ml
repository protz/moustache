type typ =
  | Arrow of typ * typ
  | Unit

type expr =
  | Lambda of string * expr
  | App of expr * expr
  | Unit

let is_arrow = function
  | Arrow _ -> true
  | _ -> false

let equal = (=)
