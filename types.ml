(* Our language of types: unit, and arrows. *)
type typ =
  | TArrow of typ * typ
  | TUnit

(* Since there are no bound variables in types, we can use structural comparison
 * for equality! *)
let equal = (=)

(* A helper function *)
let is_arrow = function
  | TArrow _ -> true
  | _ -> false
