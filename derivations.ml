open Types
open Expressions

(* An older version, commented out *)

(* We have three possible constructors for expressions, meaning that for each
 * one of them, we have a corresponding typing rule. *)
(* type rule = *)
(*   | Lambda *)
(*   | App *)
(*   | Unit *)

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

(* An expression that we successfully type-checked yields a stream of possible
 * types for it. *)
type successful_derivation =
  typ Streaml.t

(* Trying to type-check an expression may yields a successful outcome (we
 * found at least one way to type-check the expression) or nothing. *)
type outcome =
  successful_derivation option

(* Basic handling of environments. We map each variable (that is, a string) to a
 * stream of possible types for it. *)
module Env = struct

  module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

  type env = {
    env: outcome StringMap.t
  }

  let empty: env =
    { env = StringMap.empty }

  let add { env } x sd =
    { env = StringMap.add x sd env }

  let find { env } x =
    StringMap.find x env

end

(* This type-checks an application [e1 e2]. *)
let rec check_app env (e1: expr) (e2: expr): outcome =
  match check_expr env e1 with
  | None ->
      (* First premise failed *)
      None
  | Some sd1 ->
  match check_expr env e2 with
  | None ->
      (* Second premise failed *)
      None
  | Some sd2 ->
      (* Only keep arrows *)
      let sd1 = Streaml.filter is_arrow sd1 in
      (* The function application only works if the type of the arrow domain
       * matches the type of the argument. *)
      let apply t1 t2 =
        match t1, t2 with
        | TArrow (u, v), u' when equal u u' ->
            Some v
        | _ ->
            None
      in
      (* Take all possible combinations *)
      let sd = Streaml.combine apply sd1 sd2 in
      (* Turn this into an outcome, that is, return [None] if the stream is
       * empty, or [Some sd] if the stream is non-empty *)
      let sd = Streaml.lift_option sd in
      sd

and check_lambda env (x: string) (body: expr): outcome =
  (* We're dumb: we only try two possible types for [x]. Note: this is the only
   * place where a new variable is bound. *)
  let sd1 = Some (Streaml.of_list [TUnit; TArrow (TUnit, TUnit)]) in
  let env = Env.add env x sd1 in
  check_expr env body

and check_unit: outcome =
  Some (Streaml.of_list [TUnit])

and check_var env x =
  Env.find env x

and check_expr (env: Env.env) (expr: expr): outcome =
  match expr with
  | EApp (e1, e2) ->
      check_app env e1 e2
  | ELambda (x, e) ->
      check_lambda env x e
  | EUnit ->
      check_unit
  | EVar x ->
      check_var env x
