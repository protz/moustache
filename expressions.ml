(* Our language of expressions. To keep things simple, variables are represented
 * as strings. *)
type expr =
  | ELambda of string * expr
  | EApp of expr * expr
  | EUnit
  | EVar of string
