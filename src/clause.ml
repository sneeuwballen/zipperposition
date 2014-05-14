
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Clauses} *)

open Logtk

module ST = ScopedTerm
module T = FOTerm
module S = Substs
module Lit = Literal
module Lits = Literals

let stat_fresh = Util.mk_stat "fresh_clause"
let stat_clause_create = Util.mk_stat "clause_create"
let prof_clause_create = Util.mk_profiler "clause_create"

type scope = Substs.scope

(** {2 Clauses that depend on a Context} *)
module type S = sig
  module Ctx : Ctx.S

  (* TODO: maybe a global (weak) graph of parents/descendants
   * would be leaner on memory? *)

  type t
  type clause = t

  (** {2 Flags} *)

  val flag_ground : int                       (** clause is ground *)
  val flag_lemma : int                        (** clause is a lemma *)
  val flag_persistent : int                   (** clause cannot be redundant *)

  val set_flag : int -> t -> bool -> unit     (** set boolean flag *)
  val get_flag : int -> t -> bool             (** get value of boolean flag *)
  val new_flag : unit -> int                  (** new flag that can be used on clauses *)

  (** {2 Basics} *)

  val eq : t -> t -> bool         (** equality of clauses *)
  val hash : t -> int             (** hash a clause *)
  val compare : t -> t -> int     (** simple order on clauses (by ID) *)

  val id : t -> int
  val lits : t -> Literal.t array
  val parents : t -> t list

  val compact : t -> CompactClause.t (** Turn into a compact clause *)
  val is_ground : t -> bool
  val weight : t -> int

  module CHashtbl : Hashtbl.S with type key = t

  module CHashSet : sig
    type t
    val create : unit -> t
    val is_empty : t -> bool
    val member : t -> clause -> bool
    val iter : t -> (clause -> unit) -> unit
    val add : t -> clause -> unit
    val to_list : t -> clause list
  end

  val is_child_of : child:t -> t -> unit
    (** [is_child_of ~child c] is to be called to remember that [child] is a child
        of [c], is has been infered/simplified from [c] *)

  val follow_simpl : t -> t
    (** Follow the "hcsimplto" links until the clause has None *)

  val simpl_to : from:t -> into:t -> unit
    (** [simpl_to ~from ~into] sets the link of [from] to [into], so that
        the simplification of [from] into [into] is cached. *)

  module CHashcons : Hashcons.S with type elt = clause

  val create : ?parents:t list -> ?selected:BV.t ->
               Literal.t list ->
               (CompactClause.t -> Proof.t) -> t
    (** Build a new hclause from the given literals. *)

  val create_a : ?parents:t list -> ?selected:BV.t ->
                  Literal.t array ->
                  (CompactClause.t -> Proof.t) -> t
    (** Build a new hclause from the given literals. This function takes
        ownership of the input array. *)

  val of_forms : ?parents:t list -> ?selected:BV.t ->
                      Formula.FO.t list ->
                      (CompactClause.t -> Proof.t) -> t
    (** Directly from list of formulas *)

  val of_forms_axiom : ?role:string -> file:string -> name:string ->
                       Formula.FO.t list -> t
    (** Construction from formulas as axiom (initial clause) *)

  val proof : t -> Proof.t
    (** Extract its proof from the clause *)

  val stats : unit -> (int*int*int*int*int*int)
    (** hashconsing stats *)

  val is_empty : t -> bool
    (** Is the clause an empty clause? *)

  val length : t -> int
    (** Number of literals *)

  val descendants : t -> int SmallSet.t
    (** set of ID of descendants of the clause *)

  val apply_subst : renaming:Substs.Renaming.t -> Substs.t -> t -> scope -> t
    (** apply the substitution to the clause *)

  val maxlits : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are maximal under [ord] *)

  val is_maxlit : t -> scope -> Substs.t -> idx:int -> bool
    (** Is the i-th literal maximal in subst(clause)? Equivalent to
        Bitvector.get (maxlits ~ord c subst) i *)

  val eligible_res : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for resolution. THe literal has to be either maximal
        among selected literals of the same sign, if some literal is selected,
        or maximal if none is selected. *)

  val eligible_param : t -> scope -> Substs.t -> BV.t
    (** Bitvector that indicates which of the literals of [subst(clause)]
        are eligible for paramodulation. That means the literal
        is positive, no literal is selecteed, and the literal
        is maximal among literals of [subst(clause)]. *)

  val eligible_chaining : t -> scope -> Substs.t -> BV.t
    (** Bitvector of literals of [subst(clause)] that are eligible
        for equality chaining or inequality chaining. That amouns to being
        a maximal, positive inequality literal within the clause,
        and assume the clause has no selected literal. *)

  val has_selected_lits : t -> bool
    (** does the clause have some selected literals? *)

  val is_selected : t -> int -> bool
    (** check whether a literal is selected *)

  val selected_lits : t -> (Literal.t * int) list
    (** get the list of selected literals *)

  val is_unit_clause : t -> bool
    (** is the clause a unit clause? *)

  val is_oriented_rule : t -> bool
    (** Is the clause a positive oriented clause? *)

  val symbols : ?init:Symbol.Set.t -> t Sequence.t -> Symbol.Set.t
    (** symbols that occur in the clause *)

  module Seq : sig
    val lits : t -> Literal.t Sequence.t
    val terms : t -> FOTerm.t Sequence.t
    val vars : t -> FOTerm.t Sequence.t

    val abstract : t -> (bool * FOTerm.t Sequence.t) Sequence.t
      (** Easy iteration on an abstract view of literals *)
  end

  (** {2 Filter literals} *)

  module Eligible : sig
    type t = int -> Literal.t -> bool
      (** Eligibility criterion for a literal *)

    val res : clause -> t
      (** Only literals that are eligible for resolution *)

    val param : clause -> t
      (** Only literals that are eligible for paramodulation *)

    val chaining : clause -> t
      (** Eligible for chaining *)

    val eq : t
      (** Equations *)

    val ineq : clause -> t
      (** Only literals that are inequations *)

    val ineq_of : clause -> Theories.TotalOrder.t -> t
      (** Only literals that are inequations for the given ordering *)

    val arith : t

    val filter : (Literal.t -> bool) -> t

    val max : clause -> t
      (** Maximal literals of the clause *)

    val pos : t
      (** Only positive literals *)

    val neg : t
      (** Only negative literals *)

    val always : t
      (** All literals *)

    val combine : t list -> t
      (** Logical "and" of the given eligibility criteria. A literal is
          eligible only if all elements of the list say so. *)

    val ( ** ) : t -> t -> t
      (** Logical "and" *)

    val ( ++ ) : t -> t -> t
      (** Logical "or" *)

    val ( ~~ ) : t -> t
      (** Logical "not" *)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet : Set.S with type elt = t

  (** Set with access by ID, bookeeping of maximal var... *)
  module CSet : sig
    (** Set of hashconsed clauses. *)
    type t

    val empty : t
      (** the empty set *)

    val is_empty : t -> bool
      (** is the set empty? *)

    val size : t -> int
      (** number of clauses in the set *)

    val add : t -> clause -> t
      (** add the clause to the set *)

    val add_list : t -> clause list -> t
      (** add several clauses to the set *)

    val remove_id : t -> int -> t
      (** remove clause by ID *)

    val remove : t -> clause -> t
      (** remove hclause *)

    val remove_list : t -> clause list -> t
      (** remove hclauses *)

    val get : t -> int -> clause
      (** get a clause by its ID *)

    val mem : t -> clause -> bool
      (** membership test *)

    val mem_id : t -> int -> bool
      (** membership test by t ID *)

    val choose : t -> clause option
      (** Choose a clause in the set *)

    val union : t -> t -> t
      (** Union of sets *)

    val inter : t -> t -> t
      (** Intersection of sets *)

    val iter : t -> (clause -> unit) -> unit
      (** iterate on clauses in the set *)

    val iteri : t -> (int -> clause -> unit) -> unit
      (** iterate on clauses in the set with their ID *)

    val fold : t -> 'b -> ('b -> int -> clause -> 'b) -> 'b
      (** fold on clauses *)

    val to_list : t -> clause list
    val of_list : clause list -> t

    val to_seq : t -> clause Sequence.t
    val of_seq : t -> clause Sequence.t -> t
    val remove_seq : t -> clause Sequence.t -> t
    val remove_id_seq : t -> int Sequence.t -> t
  end

  (** {2 Position} *)

  module Pos : sig
    val at : t -> Position.t -> FOTerm.t
  end

  (** {2 Clauses with more data} *)

  (** Clause within which a subterm (and its position) are hilighted *)
  module WithPos : sig
    type t = {
      clause : clause;
      pos : Position.t;
      term : FOTerm.t;
    }

    val compare : t -> t -> int
    val pp : Buffer.t -> t -> unit
  end

  (** {2 IO} *)

  val pp : Buffer.t -> t -> unit
  val pp_tstp : Buffer.t -> t -> unit
  val pp_tstp_full : Buffer.t -> t -> unit  (** Print in a cnf() statement *)

  val to_string : t -> string               (** Debug printing to a  string *)
  val fmt : Format.formatter -> t -> unit   (** debug printing *)

  val pp_set : Buffer.t -> CSet.t -> unit
  val pp_set_tstp : Buffer.t -> CSet.t -> unit
end

(** {2 Type def} *)
module Make(Ctx : Ctx.S) : S with module Ctx = Ctx = struct
  module Ctx = Ctx

  type t = {
    hclits : Literal.t array;               (** the literals *)
    mutable hctag : int;                    (** unique ID of the clause *)
    mutable hcflags : int;                  (** boolean flags for the clause *)
    mutable hcselected : BV.t;              (** bitvector for selected literals *)
    mutable hcproof : Proof.t;              (** Proof of the clause *)
    mutable hcparents : t list;             (** parents of the clause *)
    mutable hcdescendants : int SmallSet.t ;(** the set of IDs of descendants of the clause *)
    mutable hcsimplto : t option;           (** simplifies into the clause *)
  }

  type clause = t

  (** {2 boolean flags} *)

  let new_flag =
    let flag_gen = Util.Flag.create () in
    fun () -> Util.Flag.get_new flag_gen

  let flag_ground = new_flag ()
  let flag_lemma = new_flag ()
  let flag_persistent = new_flag ()

  let set_flag flag c truth =
    if truth
      then c.hcflags <- c.hcflags lor flag
      else c.hcflags <- c.hcflags land (lnot flag)

  let get_flag flag c = (c.hcflags land flag) != 0

  (** {2 Hashcons} *)

  let eq hc1 hc2 = hc1.hctag = hc2.hctag

  let compare hc1 hc2 = hc1.hctag - hc2.hctag

  let hash c = Lits.hash c.hclits

  let id c = c.hctag

  let parents c = c.hcparents

  let compact c = c.hclits

  let is_ground c = get_flag flag_ground c

  let weight c = Lits.weight c.hclits

  let lits c = c.hclits

  module CHashtbl = Hashtbl.Make(struct
    type t = clause
    let hash c = hash c
    let equal c1 c2 = eq c1 c2
  end)

  module CHashSet = struct
    type t = unit CHashtbl.t
    let create () = CHashtbl.create 13
    let is_empty t = CHashtbl.length t = 0
    let member t c = CHashtbl.mem t c
    let iter t f = CHashtbl.iter (fun c _ -> f c) t
    let add t c = CHashtbl.replace t c ()
    let to_list t =
      let l = ref [] in
      iter t (fun c -> l := c :: !l);
      !l
  end

  (** {2 Utils} *)

  (** [is_child_of ~child c] is to be called to remember that [child] is a child
      of [c], is has been infered/simplified from [c] *)
  let is_child_of ~child c =
    (* update the parent clauses' sets of descendants by adding [child] *)
    let descendants = SmallSet.add c.hcdescendants child.hctag in
    c.hcdescendants <- descendants

  (* see if [c] is known to simplify into some other clause *)
  let rec _follow_simpl n c =
    if n > 10_000 then
      failwith (Util.sprintf "follow_simpl loops on %a" Lits.pp c.hclits);
    match c.hcsimplto with
    | None -> c
    | Some c' ->
      Util.debug 3 "clause %a already simplified to %a"
        Lits.pp c.hclits Lits.pp c'.hclits;
      _follow_simpl (n+1) c'

  let follow_simpl c = _follow_simpl 0 c

  (* [from] simplifies into [into] *)
  let simpl_to ~from ~into =
    let from = follow_simpl from in
    assert (from.hcsimplto = None);
    let into' = follow_simpl into in
    (* avoid cycles *)
    if from != into' then
      from.hcsimplto <- Some into

  module CHashcons = Hashcons.Make(struct
    type t = clause
    let hash c = Lits.hash c.hclits
    let equal c1 c2 = Lits.eq_com c1.hclits c2.hclits
    let tag i c = (assert (c.hctag = (-1)); c.hctag <- i)
  end)

  let __no_select = BV.empty ()

  let create_a ?parents ?selected lits proof =
    Util.enter_prof prof_clause_create;
    (* Rename variables.
    let renaming = Ctx.renaming_clear ~ctx in
    let lits = Lits.apply_subst ~renaming ~ord:ctx.Ctx.ord
      S.empty lits 0 in
    *)
    (* proof *)
    let proof' = proof lits in
    (* create the structure *)
    let c = {
      hclits = lits;
      hcflags = 0;
      hctag = (-1);
      hcselected = __no_select;
      hcproof = proof';
      hcparents = [];
      hcdescendants = SmallSet.empty ~cmp:(fun i j -> i-j);
      hcsimplto = None;
    } in
    let old_hc, c = c, CHashcons.hashcons c in
    if c == old_hc then begin
      (* select literals, if not already done *)
      begin c.hcselected <- match selected with
        | Some bv -> bv
        | None -> Ctx.select lits
      end;
      (* compute flags *)
      if Lits.is_ground lits then set_flag flag_ground c true;
      (* parents *)
      begin match parents with
      | None -> ()
      | Some parents ->
        c.hcparents <- parents;
        List.iter (fun parent -> is_child_of ~child:c parent) parents
      end
    end;
    (* return clause *)
    Util.incr_stat stat_clause_create;
    Util.exit_prof prof_clause_create;
    c

  (** Build clause from a list (delegating to create_a) *)
  let create ?parents ?selected lits proof =
    create_a ?parents ?selected (Array.of_list lits) proof

  let of_forms ?parents ?selected forms proof =
    let lits = List.map Ctx.Lit.of_form forms in
    create ?parents ?selected lits proof

  let of_forms_axiom ?(role="axiom") ~file ~name forms =
    let lits = Lits.Conv.of_forms forms in
    let proof c = Proof.mk_c_file ~role ~file ~name c in
    create_a lits proof

  let proof c = c.hcproof

  let is_empty c = Array.length c.hclits = 0

  let length c = Array.length c.hclits

  let stats () = CHashcons.stats ()

  (** descendants of the clause *)
  let descendants c = c.hcdescendants

  (** Apply substitution to the clause. Note that using the same renaming for all
      literals is important. *)
  let apply_subst ~renaming subst c scope =
    let lits = Array.map
      (fun lit -> Lit.apply_subst ~renaming subst lit scope)
      c.hclits in
    let descendants = c.hcdescendants in
    let proof = Proof.adapt_c c.hcproof in
    let new_hc = create_a ~parents:[c] lits proof in
    new_hc.hcdescendants <- descendants;
    new_hc

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are maximal under [ord] *)
  let maxlits c scope subst =
    let ord = Ctx.ord () in
    let renaming = S.Renaming.create () in
    let lits = Lits.apply_subst ~renaming subst c.hclits scope in
    Lits.maxlits ~ord lits

  (** Check whether the literal is maximal *)
  let is_maxlit c scope subst ~idx =
    let bv = maxlits c scope subst in
    BV.get bv idx

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for resolution. *)
  let eligible_res c scope subst =
    let ord = Ctx.ord () in
    let renaming = S.Renaming.create () in
    (* instantiate lits *)
    let lits = Lits.apply_subst ~renaming subst c.hclits scope in
    let selected = c.hcselected in
    let n = Array.length lits in
    (* Literals that may be eligible: all of them if none is selected,
       selected ones otherwise. *)
    let check_sign = not (BV.is_empty selected) in
    let bv = if BV.is_empty selected
      then BV.create ~size:n true
      else BV.copy selected
    in
    (* Only keep literals that are maximal. If [check_sign] is true, comparisons
       are only done between same-sign literals. *)
    for i = 0 to n-1 do
      (* i-th lit is already known not to be max? *)
      if not (BV.get bv i) then () else
      let lit = lits.(i) in
      for j = i+1 to n-1 do
        let lit' = lits.(j) in
        (* check if both lits are still potentially eligible, and have the same
           sign if [check_sign] is true. *)
        if (check_sign && Lit.is_pos lit <> Lit.is_pos lit')
            || not (BV.get bv j)
          then ()
          else match Lit.Comp.compare ~ord lits.(i) lits.(j) with
          | Comparison.Incomparable
          | Comparison.Eq -> ()     (* no further information about i-th and j-th *)
          | Comparison.Gt -> BV.reset bv j  (* j-th cannot be max *)
          | Comparison.Lt -> BV.reset bv i  (* i-th cannot be max *)
      done;
    done;
    bv

  (** Bitvector that indicates which of the literals of [subst(clause)]
      are eligible for paramodulation. *)
  let eligible_param c scope subst =
    let ord = Ctx.ord () in
    if BV.is_empty c.hcselected then begin
      (* instantiate lits *)
      let renaming = S.Renaming.create () in
      let lits = Lits.apply_subst ~renaming subst c.hclits scope in
      (* maximal ones *)
      let bv = Lits.maxlits ~ord lits in
      (* only keep literals that are positive *)
      BV.filter bv (fun i -> Lit.is_pos lits.(i));
      bv
    end else BV.empty ()  (* no eligible literal when some are selected *)

  let eligible_chaining c scope subst =
    let ord = Ctx.ord () in
    if BV.is_empty c.hcselected then begin
      let renaming = S.Renaming.create () in
      let lits = Lits.apply_subst ~renaming subst c.hclits scope in
      let bv = Lits.maxlits ~ord lits in
      (* only keep literals that are positive *)
      BV.filter bv (fun i -> Lit.is_pos lits.(i));
      (* only keep ordering lits *)
      BV.filter bv (fun i -> Lit.is_ineq lits.(i));
      bv
    end else BV.empty ()

  (** are there selected literals in the clause? *)
  let has_selected_lits c = not (BV.is_empty c.hcselected)

  (** Check whether the literal is selected *)
  let is_selected c i = BV.get c.hcselected i

  (** Indexed list of selected literals *)
  let selected_lits c = BV.selecti c.hcselected c.hclits

  (** is the clause a unit clause? *)
  let is_unit_clause c = match c.hclits with
    | [|_|] -> true
    | _ -> false

  let is_oriented_rule c =
    let ord = Ctx.ord () in
    match c.hclits with
    | [| Lit.Equation (l, r, true) |] ->
        begin match Ordering.compare ord l r with
        | Comparison.Gt
        | Comparison.Lt -> true
        | Comparison.Eq
        | Comparison.Incomparable -> false
        end
    | [| Lit.Prop (_, true) |] -> true
    | _ -> false

  let symbols ?(init=Symbol.Set.empty) seq =
    Sequence.fold
      (fun set c -> Lits.symbols ~init:set c.hclits)
      init seq

  module Seq = struct
    let lits c = Sequence.of_array c.hclits
    let terms c = lits c |> Sequence.flatMap Lit.Seq.terms
    let vars c = terms c |> Sequence.flatMap T.Seq.vars
    let abstract c =
      Lits.Seq.abstract c.hclits
  end

  (** {2 Filter literals} *)

  module Eligible = struct
    type t = int -> Lit.t -> bool

    let res c =
      let bv = eligible_res c 0 S.empty in
      fun i lit -> BV.get bv i

    let param c =
      let bv = eligible_param c 0 S.empty in
      fun i lit -> BV.get bv i

    let chaining c =
      let bv = eligible_chaining c 0 S.empty in
      fun i lit -> BV.get bv i

    let eq i lit = match lit with
      | Lit.Equation (_, _, true) -> true
      | _ -> false

    let ineq c = fun i lit -> Lit.is_ineq lit

    let ineq_of clause instance =
      fun i lit -> Lit.is_ineq_of ~instance lit

    let arith i lit = Lit.is_arith lit

    let filter f i lit = f lit

    let max c =
      let bv = Lits.maxlits ~ord:(Ctx.ord ()) c.hclits in
      fun i lit ->
        BV.get bv i

    let pos i lit = Lit.is_pos lit

    let neg i lit = Lit.is_neg lit

    let always i lit = true

    let combine l = match l with
    | [] -> (fun i lit -> true)
    | [x] -> x
    | [x; y] -> (fun i lit -> x i lit && y i lit)
    | [x; y; z] -> (fun i lit -> x i lit && y i lit && z i lit)
    | _ -> (fun i lit -> List.for_all (fun eligible -> eligible i lit) l)

    let ( ** ) f1 f2 i lit = f1 i lit && f2 i lit
    let ( ++ ) f1 f2 i lit = f1 i lit || f2 i lit
    let ( ~~ ) f i lit = not (f i lit)
  end

  (** {2 Set of clauses} *)

  (** Simple set *)
  module ClauseSet = Set.Make(struct
    type t = clause
    let compare hc1 hc2 = hc1.hctag - hc2.hctag
  end)

  (** Set with access by ID, bookeeping of maximal var... *)
  module CSet = struct
    module IntMap = Map.Make(struct
      type t = int
      let compare i j = i - j
    end)

    type t = clause IntMap.t
      (** Set of hashconsed clauses. Clauses are indexable by their ID. *)

    let empty = IntMap.empty

    let is_empty = IntMap.is_empty

    let size set = IntMap.fold (fun _ _ b -> b + 1) set 0

    let add set c = IntMap.add c.hctag c set

    let add_list set hcs =
      List.fold_left add set hcs

    let remove_id set i = IntMap.remove i set

    let remove set c = remove_id set c.hctag

    let remove_list set hcs =
      List.fold_left remove set hcs

    let get set i = IntMap.find i set

    let mem set c = IntMap.mem c.hctag set

    let mem_id set i = IntMap.mem i set

    let choose set =
      try Some (snd (IntMap.choose set))
      with Not_found -> None

    let union s1 s2 = IntMap.merge
      (fun _ c1 c2 -> match c1, c2 with
        | Some c1, Some c2 -> assert (c1 == c2); Some c1
        | Some c, None
        | None, Some c -> Some c
        | None, None -> None)
      s1 s2

    let inter s1 s2 = IntMap.merge
      (fun _ c1 c2 -> match c1, c2 with
        | Some c1, Some c2 -> assert (c1 == c2); Some c1
        | Some _, None
        | None, Some _
        | None, None -> None)
      s1 s2

    let iter set k = IntMap.iter (fun _ c -> k c) set

    let iteri set k = IntMap.iter k set

    let fold set acc f =
      let acc = ref acc in
      iteri set (fun i c -> acc := f !acc i c);
      !acc

    let to_list set =
      IntMap.fold (fun _ c acc -> c :: acc) set []

    let of_list l =
      add_list empty l

    let to_seq set =
      Sequence.from_iter
        (fun k -> iter set k)

    let of_seq set seq = Sequence.fold add set seq

    let remove_seq set seq = Sequence.fold remove set seq

    let remove_id_seq set seq = Sequence.fold remove_id set seq
  end

  (** {2 Positions in clauses} *)

  module Pos = struct
    let at c pos = Lits.Pos.at c.hclits pos
  end

  module WithPos = struct
    type t = {
      clause : clause;
      pos : Position.t;
      term : T.t;
    }
    let compare t1 t2 =
      let c = t1.clause.hctag - t2.clause.hctag in
      if c <> 0 then c else
      let c = T.cmp t1.term t2.term in
      if c <> 0 then c else
      Position.compare t1.pos t2.pos

    let pp buf t =
      Printf.bprintf buf "clause %a at pos %a" Lits.pp t.clause.hclits Position.pp t.pos
  end


  (** {2 IO} *)

  let pp buf c =
    let pp_annot selected maxlits i =
      ""^(if BV.get selected i then "+" else "")
        ^(if BV.get maxlits i then "*" else "")
    in
    (* print literals with a '*' for maximal, and '+' for selected *)
    let selected = c.hcselected in
    let max = maxlits c 0 S.empty in
    Buffer.add_char buf '[';
    Util.pp_arrayi ~sep:" | "
      (fun buf i lit ->
        let annot = pp_annot selected max i in
        Lit.pp buf lit;
        Buffer.add_string buf annot)
      buf c.hclits;
    Buffer.add_char buf ']';
    ()

  let pp_tstp buf c =
    match c.hclits with
    | [| |] -> Buffer.add_string buf "$false"
    | [| l |] -> Lit.pp_tstp buf l
    | _ -> Printf.bprintf buf "(%a)" Lits.pp_tstp c.hclits

  let pp_tstp_full buf c =
    Printf.bprintf buf "cnf(%d, plain, %a)." c.hctag pp_tstp c

  let to_string = Util.on_buffer pp

  let fmt fmt c =
    Format.pp_print_string fmt (to_string c)

  let pp_set buf set =
    Sequence.iter
      (fun c -> pp buf c; Buffer.add_char buf '\n')
      (CSet.to_seq set)

  let pp_set_tstp buf set =
    Sequence.iter
      (fun c -> pp_tstp buf c; Buffer.add_char buf '\n')
      (CSet.to_seq set)
end
