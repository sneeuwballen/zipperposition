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

(** {1 Precedence (total ordering) on symbols} *)

type symbol_status =
  | Multiset
  | Lexicographic

(** {2 Signature} *)

module type S = sig
  type symbol

  type t
    (** Total Ordering on a finite number of symbols, plus a few more
        data (weight for KBO, status for RPC) *)

  type precedence = t

  val eq : t -> t -> bool
    (** Check whether the two precedences are equal (same snapshot) *)

  val snapshot : t -> symbol list
    (** Current list of symbols, in decreasing order *)

  val compare : t -> symbol -> symbol -> int
    (** Compare two symbols using the precedence *)

  val mem : t -> symbol -> bool
    (** Is the symbol part of the precedence? *)

  val status : t -> symbol -> symbol_status
    (** Status of the symbol *)

  val weight : t -> symbol -> int
    (** Weight of a symbol (for KBO). Strictly positive int. *)

  val add_list : t -> symbol list -> t
    (** Update the precedence with the given symbols *)

  val add_seq : t -> symbol Sequence.t -> t

  val declare_status : t -> symbol -> symbol_status -> t
    (** Change the status of the given precedence *)

  module Seq : sig
    val symbols : t -> symbol Sequence.t
  end

  val pp_snapshot : Buffer.t -> symbol list -> unit
  val pp_debug : Buffer.t -> t -> unit
  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string

  (** {2 Builtin constraints} *)

  module Constr : sig
    type t = symbol -> symbol -> Comparison.t
      (** A partial order on symbols, used to make the precedence more
          precise *)

    val cluster : symbol list list -> t
      (** ordering constraint by clustering symbols by decreasing order.
          all symbols in the first clusters are bigger than those in the second, etc. *)

    val of_list : symbol list -> t
      (** symbols in the given list are in decreasing order *)

    val of_precedence : precedence -> t
      (** Copy of another precedence on the common symbols *)

    val arity : (symbol -> int) -> t
      (** decreasing arity constraint (big arity => high in precedence) *)

    val invfreq : symbol Sequence.t -> t
      (** symbols with high frequency are smaller *)

    val max : symbol list -> t
      (** maximal symbols, in decreasing order *)

    val min : symbol list -> t
      (** minimal symbols, in decreasing order *)

    val alpha : t
      (** alphabetic ordering on symbols *)
  end

  val weight_modarity : arity:(symbol -> int) -> symbol -> int
  val weight_constant : symbol -> int

  (** {2 Creation of a precedence from constraints} *)

  val create : Constr.t list -> symbol list -> t
    (** make a precedence from the given constraints. Constraints near
        the head of the list are {b more important} than constraints close
        to the tail. Only the very first constraint is assured to be totally
        satisfied if constraints do not agree with one another. *)

  val default : symbol list -> t
    (** default precedence. Default status for symbols is {!Lexicographic}. *)

  val default_seq : symbol Sequence.t -> t
    (** default precedence on the given sequence of symbols *)
end

(** {2 Functor} *)

module type SYMBOL = sig
  type t

  val eq : t -> t -> bool
  val hash : t -> int
  val cmp : t -> t -> int

  val false_ : t
  val true_ : t

  val pp : Buffer.t -> t -> unit
  val pp_debug : Buffer.t -> t -> unit
end

module Make(Sym : SYMBOL) = struct
  type symbol = Sym.t

  module Tbl = CCPersistentHashtbl.Make(struct
    type t = Sym.t
    let equal = Sym.eq
    let hash = Sym.hash
  end)

  (* used to complete orderings *)
  module PO = PartialOrder.Make(Sym)

  type t = {
    snapshot : symbol list; (* symbols by decreasing order *)
    index : int Tbl.t;      (* symbol -> index in precedence *)
    weight : symbol -> int; (* symbol -> weight *)
    status : unit Tbl.t;    (* set of multiset-status symbols *)
    constr : (symbol -> symbol -> Comparison.t) list;
      (* constraints used to build the precedence *)
  }

  type precedence = t

  let eq p1 p2 =
    try List.for_all2 (==) p1.snapshot p2.snapshot
    with Invalid_argument _ -> false

  let snapshot p = p.snapshot

  let compare p s1 s2 =
    let i1 = try Tbl.find p.index s1 with Not_found -> -1 in
    let i2 = try Tbl.find p.index s2 with Not_found -> -1 in
    let c = i2 - i1 in
    if c = 0
      then Sym.cmp s1 s2
      else c

  let mem p s = Tbl.mem p.index s

  let status p s =
    if Tbl.mem p.status s
      then Multiset
      else Lexicographic

  let weight p s = p.weight s

  let declare_status p s status =
    match status with
    | Lexicographic when not (Tbl.mem p.status s) -> p
    | Multiset when (Tbl.mem p.status s) -> p
    | Lexicographic ->
      { p with status = Tbl.remove p.status s; }
    | Multiset ->
      { p with status = Tbl.replace p.status s (); }

  module Seq = struct
    let symbols p = Sequence.of_list p.snapshot
  end

  let pp_snapshot buf s =
    Util.pp_list ~sep:" > " Sym.pp buf s

  let pp buf prec =
    Util.pp_list ~sep:" > "
      (fun buf s -> match status prec s with
        | Multiset -> Printf.bprintf buf "%a[M]" Sym.pp s
        | Lexicographic -> Sym.pp buf s)
      buf prec.snapshot

  let pp_debug buf prec =
    Util.pp_list ~sep:" > "
      (fun buf s -> match status prec s with
        | Multiset -> Printf.bprintf buf "%a[M]" Sym.pp_debug s
        | Lexicographic -> Sym.pp_debug buf s)
      buf prec.snapshot

  let to_string = Util.on_buffer pp

  let fmt fmt p = Format.pp_print_string fmt (to_string p)

  (* build a table  symbol -> i. such as if
      [tbl s = i], then w[List.nth i l = s] *)
  let _mk_table l =
    Util.list_foldi
      (fun tbl i s -> Tbl.replace tbl s i)
      (Tbl.create 7) l

  (** {3 Constraints} *)

  module Constr = struct
    type t = symbol -> symbol -> Comparison.t

    let cluster clusters =
      (* symbol -> index of cluster the symbol belongs to *)
      let tbl = Util.list_foldi
        (fun acc i cluster ->
          List.fold_left (fun acc s -> Tbl.replace acc s i) acc cluster)
        (Tbl.create 7) clusters
      in
      (* compare symbols by their index, if they have one.
          Smaller numbers are bigger symbols *)
      fun s1 s2 ->
        try
          let i1 = Tbl.find tbl s1 in
          let i2 = Tbl.find tbl s2 in
          Comparison.of_total (i2 - i1)
        with Not_found -> Comparison.Incomparable

    let of_list l =
      let tbl = _mk_table l in
      (* compare symbols by number. Smaller symbols have bigger number *)
      fun s1 s2 ->
        try
          let i1 = Tbl.find tbl s1 in
          let i2 = Tbl.find tbl s2 in
          Comparison.of_total (i2 - i1)
        with Not_found -> Comparison.Incomparable

    let of_precedence p = of_list p.snapshot

    let arity arity_of s1 s2 =
      (* bigger arity means bigger symbol *)
      Comparison.of_total (arity_of s1 - arity_of s2)

    let invfreq seq =
      (* symbol -> number of occurrences of symbol in seq *)
      let tbl = Sequence.fold
        (fun tbl s ->
          try Tbl.replace tbl s (Tbl.find tbl s + 1)
          with Not_found -> Tbl.replace tbl s 1)
        (Tbl.create 7) seq
      in
      (* compare by inverse frequency (higher frequency => smaller) *)
      fun s1 s2 ->
        try
          let n1 = Tbl.find tbl s1 in
          let n2 = Tbl.find tbl s2 in
          Comparison.of_total (n2 - n1)
        with Not_found -> Comparison.Incomparable

    let _find_noexc tbl s =
      try Some (Tbl.find tbl s)
      with Not_found -> None

    let max symbols =
      let tbl = _mk_table symbols in
      (* not found implies the symbol is smaller than maximal symbols *)
      fun s1 s2 ->
        match _find_noexc tbl s1, _find_noexc tbl s2 with
        | None, None -> Comparison.Incomparable
        | Some _, None -> Comparison.Gt
        | None, Some _ -> Comparison.Lt
        | Some i1, Some i2 -> Comparison.of_total (i2 - i1)

    let min symbols =
      let tbl = _mk_table symbols in
      (* not found implies the symbol is smaller than maximal symbols *)
      fun s1 s2 ->
        match _find_noexc tbl s1, _find_noexc tbl s2 with
        | None, None -> Comparison.Incomparable
        | Some _, None -> Comparison.Lt
        | None, Some _ -> Comparison.Gt
        | Some i1, Some i2 -> Comparison.of_total (i2 - i1)

    (* regular string ordering *)
    let alpha a b =
      Comparison.of_total (Sym.cmp a b)
  end

  (** {3 Weight} *)

  (* weight of f = arity of f + 4 *)
  let weight_modarity ~arity a = arity a + 4

  (* constant weight *)
  let weight_constant _ = 4

  (** {2 Creation of a precedence from constraints} *)

  (* order the set of symbols using the constraints *)
  let _order_symbols constrs symbols =
    let symbols = List.map fst (Tbl.to_list symbols) in
    let po = PO.create symbols in
    (* complete the partial order using constraints, starting with the
       strongest ones *)
    List.iter (fun constr -> PO.enrich po constr) constrs;
    if not (PO.is_total po)
      then failwith "Precedence: constraints are not total";
    PO.elements po

  let create constrs symbols =
    (* compute snapshot *)
    let symbols = List.fold_left
      (fun tbl s -> Tbl.replace tbl s ()) (Tbl.create 7) symbols in
    let snapshot = _order_symbols constrs symbols in
    let index = _mk_table snapshot in
    let weight = weight_constant in
    let status = Tbl.create 5 in
    { snapshot; index; weight; status; constr=constrs; }

  (* how to add a list of symbols to a precedence *)
  let add_list p l =
    if List.for_all (fun s -> Tbl.mem p.index s) l
      then p  (* already present *)
      else begin
        Util.debug 3 "add %a to the precedence" pp_snapshot l;
        let c = Constr.of_precedence p in
        (* hashtable of symbols *)
        let symbols = Sequence.fold
          (fun tbl s -> Tbl.replace tbl s ())
          (Tbl.create 13)
          Sequence.(append (of_list p.snapshot) (of_list l))
        in
        (* compute new ordering. First constraint is to be an extension
            of [p]. *)
        let snapshot = _order_symbols (c :: p.constr) symbols in
        let index = _mk_table snapshot in
        Util.debug 3 "--> snapshot %a" pp_snapshot snapshot;
        { p with snapshot; index; }
      end

  let add_seq p seq = add_list p (Sequence.to_rev_list seq)

  let default l =
    (* two constraints: false, true at end of precedence, and alpha constraint
      to be sure that the ordering is total *)
    let constrs =
      [ Constr.min [ Sym.false_; Sym.true_ ]
      ; Constr.alpha
      ]
    in
    create constrs l

  let default_seq seq =
    default (Sequence.to_rev_list seq)
end

module Default = Make(struct
  type t = Symbol.t
  let eq = Symbol.eq
  let hash = Symbol.hash
  let cmp = Symbol.cmp
  let true_ = Symbol.Base.true_
  let false_ = Symbol.Base.false_
  let pp = Symbol.pp
  let pp_debug= Symbol.pp
end)

include Default
