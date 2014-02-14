
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Prolog-like Terms}. *)

type t =
  | Var of string             (** variable *)
  | Int of Z.t                (** integer *)
  | Rat of Q.t                (** rational *)
  | App of string * t list   (** apply symbol *)
  | Bind of string * t list * t   (** bind n variables *)
  | List of t list            (** special constructor for lists *)
  | Column of t * t           (** t:t (useful for typing, e.g.) *)

type term = t

let __to_int = function
  | Var _ -> 0
  | Int _ -> 1
  | Rat _ -> 2
  | App _ -> 3
  | Bind _ -> 4
  | List _ -> 5
  | Column _ -> 6

let rec cmp t1 t2 = match t1, t2 with
  | Var s1, Var s2 -> String.compare s1 s2
  | Int i1, Int i2 -> Z.compare i1 i2
  | Rat n1, Rat n2 -> Q.compare n1 n2
  | App (s1,l1), App (s2, l2) ->
    let c = String.compare s1 s2 in
    if c = 0
    then Util.lexicograph cmp l1 l2
    else c
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
    let c = String.compare s1 s2 in
    if c = 0
    then
      let c' = cmp t1 t2 in
      if c' = 0
      then Util.lexicograph cmp v1 v2
      else c'
    else c
  | Column (x1,y1), Column (x2,y2) ->
    let c = cmp x1 x2 in
    if c = 0 then cmp y1 y2 else c
  | _ -> __to_int t1 - __to_int t2

let eq t1 t2 = cmp t1 t2 = 0

let rec hash t = match t with
  | Var s -> Hash.hash_string s
  | Int i -> Z.hash i
  | Rat n -> Hash.hash_string (Q.to_string n)  (* TODO: find better *)
  | App (s, []) -> Hash.hash_string s
  | App (s, l) ->
    Hash.hash_list hash (Hash.hash_string s) l
  | List l -> Hash.hash_list hash 0x42 l
  | Bind (s,v,t') ->
    let h = Hash.combine (Hash.hash_string s) (hash t') in
    Hash.hash_list hash h v
  | Column (x,y) -> Hash.combine (hash x) (hash y)

let var s = Var s
let int_ i = Int i
let of_int i = Int (Z.of_int i)
let rat n = Rat n
let app s l  = App(s,l)
let const s = app s []
let bind s v l = Bind(s,v,l)
let list_ l = List l
let nil = list_ []
let column x y = Column(x,y)

let is_var = function
  | Var _ -> true
  | _ -> false

module Set = Sequence.Set.Make(struct
  type t = term
  let compare = cmp
end)
module Map = Sequence.Map.Make(struct
  type t = term
  let compare = cmp
end)

module Tbl = Hashtbl.Make(struct
  type t = term
  let hash = hash
  let equal = eq
end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      match t with
      | Var _ | Int _ | Rat _ -> ()
      | List l
      | App (_, l) -> List.iter iter l
      | Bind (_, v, t') -> List.iter iter v; iter t'
      | Column(x,y) -> k x; k y
    in iter t

  let vars t = subterms t |> Sequence.filter is_var

  let add_set s seq =
    Sequence.fold (fun set x -> Set.add x set) s seq

  let subterms_with_bound t k =
    let rec iter bound t =
      k (t, bound);
      match t with
      | Var _ | Int _ | Rat _ -> ()
      | List l
      | App (_, l) -> List.iter (iter bound) l
      | Bind (_, v, t') ->
          (* add variables of [v] to the set *)
          let bound' = List.fold_left
            (fun set v -> add_set set (vars v))
            bound v
          in
          iter bound' t'
      | Column(x,y) -> k (x, bound); k (y, bound)
    in iter Set.empty t

  let free_vars t =
    subterms_with_bound t
      |> Sequence.fmap (fun (v,bound) ->
          if is_var v && not (Set.mem v bound)
          then Some v
          else None)

  let symbols t = subterms t
      |> Sequence.fmap (function
        | App (s, _) -> Some s
        | Bind (s, _, _) -> Some s
        | _ -> None)
end

let ground t = Seq.vars t |> Sequence.is_empty

let close_all s t =
  let vars = Seq.free_vars t
    |> Seq.add_set Set.empty
    |> Set.elements
  in
  bind s vars t

let rec pp buf t = match t with
  | Var s -> Buffer.add_string buf s
  | Int i -> Buffer.add_string buf (Z.to_string i)
  | Rat i -> Buffer.add_string buf (Q.to_string i)
  | List l ->
      Buffer.add_char buf '[';
      Util.pp_list ~sep:"," pp buf l;
      Buffer.add_char buf ']'
  | App (s, []) -> Buffer.add_string buf s
  | App (s, l) ->
      Buffer.add_string buf s;
      Buffer.add_char buf '(';
      Util.pp_list ~sep:"," pp buf l;
      Buffer.add_char buf ')'
  | Bind (s, vars, t') ->
      Buffer.add_string buf s;
      Buffer.add_char buf '[';
      Util.pp_list ~sep:"," pp buf vars;
      Buffer.add_string buf "]:";
      pp buf t'
  | Column(x,y) ->
      pp buf x;
      Buffer.add_char buf ':';
      pp buf y

let to_string = Util.on_buffer pp
let fmt fmt t = Format.pp_print_string fmt (to_string t)
