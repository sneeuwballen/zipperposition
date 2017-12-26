
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Positions in terms, clauses...} *)

(** A position is a path in a tree *)
type t =
  | Stop
  | Type of t (** Switch to type *)
  | Left of t (** Left term in curried application *)
  | Right of t (** Right term in curried application, and subterm of binder *)
  | Head of t (** Head of uncurried term *)
  | Arg of int * t (** argument term in uncurried term, or in multiset *)
  | Body of t (** Body of binder *)

type position = t

let stop = Stop
let type_ pos = Type pos
let left pos = Left pos
let right pos = Right pos
let head pos = Head pos
let arg i pos = Arg (i, pos)
let body pos = Body pos

let compare = Pervasives.compare
let equal p1 p2 = compare p1 p2 = 0
let hash p = Hashtbl.hash p

let rev pos =
  let rec aux acc pos = match pos with
    | Stop -> acc
    | Type pos' -> aux (Type acc) pos'
    | Left pos' -> aux (Left acc) pos'
    | Right pos' -> aux (Right acc) pos'
    | Head pos' -> aux (Head acc) pos'
    | Arg (i, pos') -> aux (Arg (i,acc)) pos'
    | Body pos' -> aux (Body acc) pos'
  in
  aux Stop pos

let opp = function
  | Left p -> Right p
  | Right p -> Left p
  | pos -> pos

(* Recursive append *)
let rec append p1 p2 = match p1 with
  | Stop -> p2
  | Type p1' -> Type (append p1' p2)
  | Left p1' -> Left (append p1' p2)
  | Right p1' -> Right (append p1' p2)
  | Head p1' -> Head (append p1' p2)
  | Arg(i, p1') -> Arg (i,append p1' p2)
  | Body p1' -> Body (append p1' p2)

let rec pp out pos = match pos with
  | Stop -> CCFormat.string out "ε"
  | Type p' -> CCFormat.string out "τ."; pp out p'
  | Left p' -> CCFormat.string out "←."; pp out p'
  | Right p' -> CCFormat.string out "→."; pp out p'
  | Head p' -> CCFormat.string out "@."; pp out p'
  | Arg (i,p') -> Format.fprintf out "%d." i; pp out p'
  | Body p' -> Format.fprintf out "β."; pp out p'

let to_string = CCFormat.to_string pp

let rec is_prefix p1 p2 : bool = match p1, p2 with
  | Stop, _ -> true
  | Left l1, Left l2
  | Right l1, Right l2
  | Head l1, Head l2
  | Body l1, Body l2
  | Type l1, Type l2
    ->
    is_prefix l1 l2
  | Arg (i1,l1), Arg (i2,l2) -> i1=i2 && is_prefix l1 l2
  | Left _, _
  | Right _, _
  | Head _, _
  | Body _, _
  | Arg _, _
  | Type _, _
    -> false

let is_strict_prefix p1 p2 = not (equal p1 p2) && is_prefix p1 p2

module Map = struct
  include CCMap.Make(struct
      type t = position
      let compare = compare
    end)

  let prune_subsumed (m:_ t): _ t =
    filter
      (fun k _ -> not (exists (fun k' _ -> is_strict_prefix k' k) m))
      m
end

(** {2 Position builder}

    We use an adaptation of difference lists for this tasks *)

module Build = struct
  type t =
    | E (** Empty (identity function) *)
    | P of position * t (** Pre-pend given position, then apply previous builder *)
    | N of (position -> position) * t
    (** Apply function to position, then apply linked builder *)

  let empty = E

  let of_pos p = P (p, E)

  (* how to apply a difference list to a tail list *)
  let rec apply_rec tail b = match b with
    | E -> tail
    | P (pos0,b') -> apply_rec (append pos0 tail) b'
    | N (f, b') -> apply_rec (f tail) b'

  let apply_flip b pos = apply_rec pos b

  let to_pos b = apply_rec stop b

  let suffix b pos =
    (* given a suffix, first append pos to it, then apply b *)
    N ((fun pos0 -> append pos pos0), b)

  let prefix pos b =
    (* tricky: this doesn't follow the recursive structure. Hence we
        need to first apply b totally, then pre-prend pos *)
    N ((fun pos1 -> append pos (apply_rec pos1 b)), E)

  let append p1 p2 = N(apply_flip p2, p1)

  let left b = N (left, b)
  let right b = N (right, b)
  let type_ b = N (type_, b)
  let head b = N(head, b)
  let arg i b = N(arg i, b)
  let body b = N(body, b)

  let pp out t = pp out (to_pos t)
  let to_string t = to_string (to_pos t)
end

(** {2 Pairing of value with Pos} *)

module With = struct
  type 'a t = 'a * position

  let get = fst
  let pos = snd

  let make x p : _ t = x, p
  let of_pair p = p

  let map_pos f (x,pos) = x, f pos

  let map f (x,pos) = f x, pos

  module Infix = struct
    let (>|=) x f = map f x
  end
  include Infix

  let equal f t1 t2 = f (get t1) (get t2) && equal (pos t1) (pos t2)
  let compare f t1 t2 =
    let c = f (get t1) (get t2) in
    if c=0 then compare (pos t1) (pos t2) else c
  let hash f t = Hash.combine3 41 (f (get t)) (hash (pos t))
  let pp f out t =
    CCFormat.fprintf out "(@[:pos %a :in %a@])" pp (pos t) f (get t)
end
