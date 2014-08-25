open Derivations
open Types
open Expressions

module Printers = struct

  let rec string_of_type (t: typ) =
    match t with
    | TArrow (t1, t2) ->
        (string_of_type t1) ^ " -> " ^ (string_of_type t2)
    | TUnit ->
        "()"

  (* Prints the contents of a string stream, one item per line. *)
  let rec dump_stream (s: string Streaml.t) =
    let open Streaml in
    match next s with
    | Cons (hd, tl) ->
        Printf.printf "%s\n%!" hd;
        dump_stream tl
    | Nil ->
        ()

  (* Prints "Some" on a single line followed by the contents of the string
   * stream, or "None". *)
  let dump_outcome = function
    | Some s ->
        Printf.printf "Some\n%!";
        dump_stream s;
        Printf.printf "\n%!"
    | None ->
        Printf.printf "None\n%!";
        Printf.printf "\n%!"

end


(* f = λx.x *)
let f =
  let x = "x" in
  ELambda (x, EVar x)

(* g = λf.λx.f x *)
let g =
  let f = "f" and x = "x" in
  ELambda (f, ELambda (x,
    EApp (EVar f, EVar x)
  ))


let _ =
  (* Type-check [g], convert to strings all the resulting types, print them. *)
  let s = check_expr empty g in
  let s = Option.map (Streaml.map string_of_type) s in
  Printf.printf "Types for g:\n";
  dump_outcome s;
  (* Same thing with [f]. *)
  let s = check_expr empty f in
  let s = Option.map (Streaml.map string_of_type) s in
  Printf.printf "Types for f:\n";
  dump_outcome s
