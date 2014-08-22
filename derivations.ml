open Types

(* An older version, commented out *)

(* type successful_derivation = *)
(*   | Success of rule * (successful_derivation list * typ) stream *)

(* and failed_derivation = *)
(*   | FailedConclusion of rule * successful_derivation stream list *)
(*   | FailedPremise of rule * int * failed_derivation *)

(* type outcome = *)
(*   | Success of successful_derivation *)
(*   | Failure of failed_derivation *)


(* -------------------------------------------------------------------------- *)


(* Simplified version of the earlier discussion *)

(* We have three possible constructors for expressions, meaning that for each
 * one of them, we have a corresponding typing rule. *)
type rule =
  | Lambda
  | App
  | Unit

(* An expression that we successfully type-checked yields a stream of possible
 * types for it. *)
type successful_derivation =
  typ Stream.t

(* Trying to type-check an expression may yields a successful outcome (we
 * found at least one way to type-check the expression) or nothing. *)
type outcome =
  successful_derivation option

let check_app (e1: expr) (e2: expr): outcome =
  match check_expr e1 with
  | None ->
      (* First premise failed *)
      None
  | Some sd1 ->
  match check_expr e2 with
  | None ->
      (* Second premise failed *)
      None
  | Some sd2 ->
      (* Only keep arrows *)
      let sd1 = Stream.filter is_arrow sd1 in
      (* The function application only works if the type of the arrow domain
       * matches the type of the argument. *)
      let apply t1 t2 =
        match t1, t2 with
        | TyArrow (u, v), u' when equal u u' ->
            Some v
        | _ ->
            None
      (* Take all possible combinations *)
      let sd = Stream.combine apply sd1 sd2 in
      (* Turn this into an outcome, that is, return [None] if the stream is
       * empty, or [Some sd] if the stream is non-empty *)
      let sd = Stream.lift_option sd in
      sd
