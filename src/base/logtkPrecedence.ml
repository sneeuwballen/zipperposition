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

(** {1 LogtkPrecedence (total ordering) on symbols} *)

type symbol_status = LogtkPrecedence_intf.symbol_status =
  | Multiset
  | Lexicographic

let section = LogtkUtil.Section.(make ~parent:logtk "precedence")

(** {2 LogtkSignature} *)

module type S = LogtkPrecedence_intf.S

(** {2 Functor} *)

module type SYMBOL = sig
  type t

  val eq : t -> t -> bool
  val hash : t -> int
  val cmp : t -> t -> int

  val false_ : t
  val true_ : t

  val pp : t CCFormat.printer
  val pp_debug : t CCFormat.printer
end

module Make(Sym : SYMBOL) = struct
  type symbol = Sym.t

  module Tbl = CCPersistentHashtbl.Make(struct
    type t = Sym.t
    let equal = Sym.eq
    let hash = Sym.hash
  end)

  (* used to complete orderings *)
  module PO = LogtkPartialOrder.Make(Sym)

  (* TODO: make the [index] a lazy instance of [Tbl.t], to be computed
    on demand, to simplify *)

  type t = {
    snapshot : symbol list; (* symbols by decreasing order *)
    index : int Tbl.t;      (* symbol -> index in precedence *)
    weight : symbol -> int; (* symbol -> weight *)
    status : unit Tbl.t;    (* set of multiset-status symbols *)
    constr : (symbol -> symbol -> LogtkComparison.t) list;
      (* constraints used to build the precedence *)
  }

  type precedence = t

  let eq p1 p2 =
    try List.for_all2 Sym.eq p1.snapshot p2.snapshot
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

  let pp_snapshot out s = CCFormat.list ~sep:" > " Sym.pp out s

  let pp out prec =
    CCFormat.list ~sep:" > "
      (fun out s -> match status prec s with
        | Multiset -> Format.fprintf out "%a[M]" Sym.pp s
        | Lexicographic -> Sym.pp out s)
      out prec.snapshot

  let pp_debug out prec =
    CCFormat.list ~sep:" > "
      (fun out s -> match status prec s with
        | Multiset -> Format.fprintf out "%a[M]" Sym.pp_debug s
        | Lexicographic -> Sym.pp_debug out s)
      out prec.snapshot

  let to_string = CCFormat.to_string pp

  (* build a table  symbol -> i. such as if
      [tbl s = i], then w[List.nth i l = s] *)
  let _mk_table l =
    CCList.Idx.foldi
      (fun tbl i s -> Tbl.replace tbl s i)
      (Tbl.create 7) l

  (** {3 Constraints} *)

  module Constr = struct
    module C = LogtkComparison

    type t = symbol -> symbol -> C.t

    let cluster clusters =
      (* symbol -> index of cluster the symbol belongs to *)
      let tbl = CCList.Idx.foldi
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
          C.of_total (i2 - i1)
        with Not_found -> C.Incomparable

    let of_list l =
      let tbl = _mk_table l in
      (* compare symbols by number. Smaller symbols have bigger number *)
      fun s1 s2 ->
        try
          let i1 = Tbl.find tbl s1 in
          let i2 = Tbl.find tbl s2 in
          C.of_total (i2 - i1)
        with Not_found -> C.Incomparable

    let of_precedence p = of_list p.snapshot

    let arity arity_of s1 s2 =
      (* bigger arity means bigger symbol *)
      C.of_total (arity_of s1 - arity_of s2)

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
          if n1=n2 then C.Incomparable
          else if n1<n2 then C.Gt
          else C.Lt
        with Not_found ->
          if Sym.eq s1 s2 then C.Eq else C.Incomparable

    let _find_noexc tbl s =
      try Some (Tbl.find tbl s)
      with Not_found -> None

    let max symbols =
      let tbl = _mk_table symbols in
      (* not found implies the symbol is smaller than maximal symbols *)
      fun s1 s2 ->
        match _find_noexc tbl s1, _find_noexc tbl s2 with
        | None, None -> C.Incomparable
        | Some _, None -> C.Gt
        | None, Some _ -> C.Lt
        | Some i1, Some i2 -> C.of_total (i2 - i1)

    let min symbols =
      let tbl = _mk_table symbols in
      (* not found implies the symbol is smaller than maximal symbols *)
      fun s1 s2 ->
        match _find_noexc tbl s1, _find_noexc tbl s2 with
        | None, None -> C.Incomparable
        | Some _, None -> C.Lt
        | None, Some _ -> C.Gt
        | Some i1, Some i2 -> C.of_total (i2 - i1)

    (* regular string ordering *)
    let alpha a b =
      C.of_total (Sym.cmp a b)
  end

  (** {3 Weight} *)

  type weight_fun = symbol -> int

  (* weight of f = arity of f + 4 *)
  let weight_modarity ~arity a = arity a + 4

  (* constant weight *)
  let weight_constant _ = 4

  let set_weight p weight = {p with weight; }

  (** {2 Creation of a precedence from constraints} *)

  (* order the set of symbols using the constraints *)
  let order_symbols_ constrs symbols =
    let symbols = List.map fst (Tbl.to_list symbols) in
    let po = PO.create symbols in
    (* complete the partial order using constraints, starting with the
       strongest ones *)
    List.iter (fun constr -> PO.enrich po constr) constrs;
    if not (PO.is_total po) then (
      match PO.is_total_details po with
        | `total -> assert false
        | `eq (s1, s2) ->
            let msg = CCFormat.sprintf
              "LogtkPrecedence: symbols %a and %a made equal"
              Sym.pp s1 Sym.pp s2 in
            failwith msg
        | `unordered (s1, s2) ->
            let msg = CCFormat.sprintf
              "LogtkPrecedence: symbols %a and %a not ordered by constraints"
              Sym.pp s1 Sym.pp s2 in
            failwith msg
    );
    PO.elements po

  let create ?(weight=weight_constant) constrs symbols =
    (* compute snapshot *)
    let symbols = List.fold_left
      (fun tbl s -> Tbl.replace tbl s ()) (Tbl.create 7) symbols in
    let snapshot = order_symbols_ constrs symbols in
    let index = _mk_table snapshot in
    let status = Tbl.create 5 in
    { snapshot; index; weight; status; constr=constrs; }

  let create_sort ?weight l symbols =
    let l = List.sort (fun (p1,_)(p2,_) -> CCInt.compare p1 p2) l in
    let l = List.map snd l in
    create ?weight l symbols

  (* how to add a list of symbols to a precedence *)
  let add_list p l =
    if List.for_all (fun s -> Tbl.mem p.index s) l
      then p  (* already present *)
      else begin
        LogtkUtil.debug ~section 3 "add %a to the precedence" (fun k->k pp_snapshot l);
        let c = Constr.of_precedence p in
        (* hashtable of symbols *)
        let symbols = Sequence.fold
          (fun tbl s -> Tbl.replace tbl s ())
          (Tbl.create 13)
          Sequence.(append (of_list p.snapshot) (of_list l))
        in
        (* compute new ordering. First constraint is to be an extension
            of [p]. *)
        let snapshot = order_symbols_ (c :: p.constr) symbols in
        let index = _mk_table snapshot in
        LogtkUtil.debug ~section 3 "--> snapshot %a" (fun k->k pp_snapshot snapshot);
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

  let constr_list p = p.constr

  let with_constr_list p constrs =
    create ~weight:p.weight constrs (snapshot p)
end

module Default = Make(struct
  type t = LogtkSymbol.t
  let eq = LogtkSymbol.eq
  let hash = LogtkSymbol.hash
  let cmp = LogtkSymbol.cmp
  let true_ = LogtkSymbol.Base.true_
  let false_ = LogtkSymbol.Base.false_
  let pp = LogtkSymbol.pp
  let pp_debug= LogtkSymbol.pp
end)

include Default
