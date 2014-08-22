type typ =
  | TArrow of typ * typ
  | TUnit

type expr =
  | ELambda of string * expr
  | EApp of expr * expr
  | EUnit
  | EVar of string

let is_arrow = function
  | TArrow _ -> true
  | _ -> false

let equal = (=)
