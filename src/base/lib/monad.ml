
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

(** {1 Utils linked to monads} *)

(** {2 Signature of a monad} *)

module type S = sig
  type 'a t

  val return : 'a -> 'a t

  val map : 'a t -> ('a -> 'b) -> 'b t
    (** Functorial map *)

  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

(** {2 Monadic traversal}
This functor allows to build fold and map functions with a monadic interface.
*)

module type TRAVERSE = sig
  type 'a monad

  val fold : 'a Sequence.t -> 'b monad -> ('b -> 'a -> 'b monad) -> 'b monad
  val fold_l : 'a list -> 'b monad -> ('b -> 'a -> 'b monad) -> 'b monad

  val map_l : 'a list -> ('a -> 'b monad) -> 'b list monad

  val seq : 'a monad list -> 'a list monad
end

module Traverse(M : S) = struct
  open M

  type 'a monad = 'a M.t

  let fold seq acc f =
    Sequence.fold
      (fun acc x -> acc >>= fun acc -> f acc x)
      acc seq

  let fold_l l acc f =
    List.fold_left
      (fun acc x -> acc >>= fun acc -> f acc x)
      acc l

  let map_l l f =
    let rec map l = match l with
      | [] -> return []
      | x::l' ->
        f x >>= fun x' ->
        map l' >>= fun l' ->
        return (x' :: l')
    in
    map l

  let rec seq l = match l with
    | [] -> return []
    | x::l' ->
      x >>= fun x' ->
      seq l' >>= fun l'' ->
      M.return (x' :: l'')
end

(** {2 Option Monad} *)

module Opt = struct
  type 'a t = 'a option

  let maybe opt x = match opt with
    | Some y -> y
    | None -> x

  let (>>=) opt f = match opt with
  | None -> None
  | Some x -> f x

  let return x = Some x

  let map x f = match x with
    | None -> None
    | Some x' -> Some (f x')

  let get = function
  | Some x -> x
  | None -> invalid_arg "Opt.get"

  let is_some = function | Some _ -> true | None -> false
  let is_none = function | None -> true | Some _ -> false

  exception LocalExit

  let fold seq acc f = match acc with
  | None -> None
  | Some acc ->
    try
      let x = Sequence.fold
        (fun acc x -> match f acc x with
          | None -> raise LocalExit
          | Some y -> y)
        acc seq
      in Some x
    with LocalExit -> None

  let fold_l l = fold (Sequence.of_list l)

  let map_l l f =
    let rec map acc l = match l with
    | [] -> Some (List.rev acc)
    | x::l' ->
        match f x with
        | None -> None
        | Some y -> map (y::acc) l'
    in map [] l

  let seq l =
    let rec iter acc l = match l with
    | [] -> Some (List.rev acc)
    | None::_ -> None
    | Some x::l' -> iter (x::acc) l'
    in iter [] l
end

(** {2 Error monad} *)

module Err = struct
  type 'a err =
    | Ok of 'a
    | Error of string

  type 'a t = 'a err

  let return x = Ok x

  let map x f = match x with
    | Ok x -> Ok (f x)
    | Error s -> Error s

  let guard ?(print=Printexc.to_string) f =
    try
      Ok (f ())
    with e ->
      Error (print e)

  let fail s = Error s
  let fail_exn ex = Error (Printexc.to_string ex)

  let (>>=) x f = match x with
    | Ok x' -> f x'
    | Error s -> Error s

  let to_opt = function
    | Ok x -> Some x
    | Error _ -> None

  exception LocalExit of string

  let fold seq acc f =
    match acc with Error s -> acc
    | Ok acc ->
    try
      let x = Sequence.fold
        (fun acc x -> match f acc x with
          | Error s -> raise (LocalExit s)
          | Ok y -> y)
        acc seq
      in Ok x
    with LocalExit s -> Error s

  let fold_l l = fold (Sequence.of_list l)

  let map_l l f =
    let rec map acc l = match l with
    | [] -> Ok (List.rev acc)
    | x::l' ->
        match f x with
        | Error s -> Error s
        | Ok y -> map (y::acc) l'
    in map [] l

  let seq l =
    let rec iter acc l = match l with
    | [] -> Ok (List.rev acc)
    | Error s::_ -> Error s
    | Ok x::l' -> iter (x::acc) l'
    in iter [] l
end

(** {2 List monad} *)

module L = struct
  module Inner = struct
    type 'a t = 'a list

    let return x = [x]

    let map x f = List.map f x

    let (>>=) l f =
      let rec expand l = match l with
      | [] -> []
      | x::l' ->
        List.rev_append (f x) (expand l')
      in
      expand l
  end
  include Inner
  include Traverse(Inner)
end

(** {2 Composition monad} *)

module Fun(Domain : sig type t end) = struct
  type domain = Domain.t
  type 'a fun_ = domain -> 'a
  type 'a t = 'a fun_

  let return x _ = x

  let (>>=) f1 f2 x =
    f2 (f1 x) x

  let map f f1 x = f1 (f x)

  let fold (seq:'a Sequence.t) (acc:'b t) (f:'b -> 'a -> 'b t) =
    Sequence.fold
      (fun acc x ->
        fun elt -> (f (acc elt) x) elt)
      acc seq

  let fold_l l = fold (Sequence.of_list l)

  let map_l l f elt = List.map (fun x -> f x elt) l

  let seq l elt =
    List.map (fun f -> f elt) l
end
