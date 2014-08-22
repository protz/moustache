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
(* type rule = *)
(*   | Lambda *)
(*   | App *)
(*   | Unit *)

(* An expression that we successfully type-checked yields a stream of possible
 * types for it. *)
type successful_derivation =
  typ Streaml.t

(* Trying to type-check an expression may yields a successful outcome (we
 * found at least one way to type-check the expression) or nothing. *)
type outcome =
  successful_derivation option

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

type env = successful_derivation StringMap.t

let empty: env =
  StringMap.empty

let add env x sd =
  StringMap.add x sd env

let find env x =
  StringMap.find x env

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
  (* We're dumb: we only try two possible types for [x] *)
  let sd1 = Streaml.of_list [TUnit; TArrow (TUnit, TUnit)] in
  let env = add env x sd1 in
  check_expr env body

and check_unit: outcome =
  Some (Streaml.of_list [TUnit])

and check_var env x =
  let sd = find env x in
  assert ((Streaml.next sd) <> Streaml.Nil);
  Some sd

and check_expr (env: env) (expr: expr): outcome =
  match expr with
  | EApp (e1, e2) ->
      check_app env e1 e2
  | ELambda (x, e) ->
      check_lambda env x e
  | EUnit ->
      check_unit
  | EVar x ->
      check_var env x
