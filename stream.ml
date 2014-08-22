(*****************************************************************************)
(*  Mezzo, a programming language based on permissions                       *)
(*  Copyright (C) 2011, 2012 Jonathan Protzenko and François Pottier         *)
(*                                                                           *)
(*  This program is free software: you can redistribute it and/or modify     *)
(*  it under the terms of the GNU General Public License as published by     *)
(*  the Free Software Foundation, either version 3 of the License, or        *)
(*  (at your option) any later version.                                      *)
(*                                                                           *)
(*  This program is distributed in the hope that it will be useful,          *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(*  GNU General Public License for more details.                             *)
(*                                                                           *)
(*  You should have received a copy of the GNU General Public License        *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
(*                                                                           *)
(*****************************************************************************)

type 'a t = 'a node Lazy.t

and 'a node =
  | Nil
  | Cons of 'a * 'a t

let nil =
  lazy Nil

let cons hd tl =
  lazy (Cons (hd, tl))

let next =
  Lazy.force

let tl (l: 'a t): 'a t =
  match next l with
  | Cons (_, tl) ->
      tl
  | Nil ->
      raise (Failure "tl")

let hd (l: 'a t): 'a =
  match next l with
  | Cons (hd, _) ->
      hd
  | Nil ->
      raise (Failure "hd")

let one (r: 'a): 'a t =
  cons r nil

let two (x: 'a) (y: 'a): 'a t =
  cons x (cons y nil)

let rec filter (f: 'a -> bool) (l: 'a t): 'a t =
  lazy begin match next l with
  | Nil ->
      Nil
  | Cons (hd, tl) ->
      if f hd then
        Cons (hd, filter f tl)
      else
        next (filter f tl)
  end

let flatten (ls: 'a t list): 'a t =
  lazy begin
    let rec flatten_aux (l: 'a t) (ls: 'a t list): 'a t =
      lazy begin match next l with
      | Nil ->
          begin match ls with
          | [] ->
              Nil
          | l :: ls ->
              next (flatten_aux l ls)
          end
      | Cons (hd, tl) ->
          Cons (hd, flatten_aux tl ls)
      end
    in
    match ls with
    | [] ->
        Nil
    | l :: ls ->
        next (flatten_aux l ls)
  end


let rec map (f: 'a -> 'b) (l: 'a t): 'b t =
  lazy begin match next l with
  | Nil ->
      Nil
  | Cons (hd, tl) ->
      Cons (f hd, map f tl)
  end

let concat (outer: 'a t t): 'a t =
  let rec concat_aux (inner: 'a t) (outer: 'a t t): 'a t =
    lazy begin match next inner with
    | Cons (head, tail) ->
        Cons (head, concat_aux tail outer)
    | Nil ->
        match next outer with
        | Nil ->
            Nil
        | Cons (head, tail) ->
            next (concat_aux head tail)
    end
  in
  concat_aux nil outer

let append (l1: 'a t) (l2: 'a t): 'a t =
  concat (two l1 l2)

let rec iter (f: 'a -> unit) (l: 'a t): unit =
  (* Funny story: ignore (map f l) is not acceptable since it never computes
   * anything! *)
  match next l with
  | Nil ->
      ()
  | Cons (hd, tl) ->
      f hd; iter f tl


let find (type elt) (f: elt -> bool) (l: elt t): elt =
  let module M = struct exception Found of elt end in
  try
    iter (fun elt -> if f elt then raise (M.Found elt) else ()) l;
    raise Not_found
  with M.Found elt ->
    elt

let exists (f: 'a -> bool) (l: 'a t): bool =
  try ignore (find f l); true
  with Not_found -> false

let rec map_filter (f: 'a -> 'b option) (l: 'a t): 'b t =
  lazy begin match next l with
  | Nil ->
      Nil
  | Cons (hd, tl) ->
      match f hd with
      | Some hd ->
          Cons (hd, map_filter f tl)
      | None ->
          next (map_filter f tl)
  end

(* Examines [l] and, if [l] contains at least one element that is not [None],
 * returns [Some l], else, returns [None] *)
let lift_option (l: 'a option t): 'a t option =
  let l = map_filter (fun x -> x) l in
  match next l with
  | Cons _ ->
      Some l
  | Nil ->
      None

let rec combine (f: 'a -> 'b -> 'c) (l1: 'a t) (l2: 'b t): 'c t =
  begin match next l1 with
  | Cons (hd1, tl1) ->
      append (map (f hd1) l2) (combine f tl1 l2)
  | Nil ->
      nil
  end
