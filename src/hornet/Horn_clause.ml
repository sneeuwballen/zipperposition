
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

open Libzipperposition
open Hornet_types

module Fmt = CCFormat
module Pos = Position
module PW = Position.With
module BV = CCBV

type t = Hornet_types.horn_clause
type horn_clause = t

(** {2 Basics} *)


(* Some clauses are added and removed several times. We keep a
   cycle counter to distinguish clauses dead at time n (which can
   be alive at time n+1), from clauses dead at time n+1, that will
   not be active anymore until cycle n+2. *)
let cycle : int ref = ref 0

let equal a b = a.hc_id = b.hc_id
let hash a = Hash.int a.hc_id
let compare a b = CCInt.compare a.hc_id b.hc_id

let pp = Hornet_types_util.pp_hclause
let to_string = Fmt.to_string pp

let head c = c.hc_head
let body c = c.hc_body
let proof c = c.hc_proof
let trail c = c.hc_trail
let label c = c.hc_label
let constr c = c.hc_constr
let unordered_depth c = c.hc_unordered_depth
let status c = c.hc_status

let set_status c new_st new_cycle =
  let old_st, old_cycle = c.hc_status in
  assert (old_cycle <= new_cycle);
  begin match old_st, new_st with
    | HC_dead, HC_alive when old_cycle < new_cycle ->
      () (* only fine for new cycle *)
    | HC_alive, HC_dead
    | HC_dead, HC_dead -> ()
    | _ ->
      Util.errorf
        ~where:"HC.set_status"
        "for `@[%a@]`,@ wrong change@ @[<2>`%a`[time %d]@ -> `%a`[time %d]@]"
        pp c
        Hornet_types_util.pp_hc_status old_st old_cycle
        Hornet_types_util.pp_hc_status new_st new_cycle
  end;
  c.hc_status <- (new_st,new_cycle);
  ()

(* register the clause in each of its trail's boolean literal
   and labelled clause.
   That way, when the literal is backtracked, the clause can be removed *)
let register_ (c:t): unit =
  let old_st, old_cycle = c.hc_status in
  begin match old_st with
    | HC_alive -> ()
    | HC_dead when old_cycle = !cycle -> () (* up-to-date *)
    | HC_dead ->
      assert (old_cycle < !cycle);
      (* clause is now alive again, with new cycle *)
      set_status c HC_alive !cycle;
      (* register to trail *)
      List.iter
        (fun (lazy b_lit) -> match b_lit.bl_atom with
           | A_box_clause r ->
             r.bool_box_depends <- c :: r.bool_box_depends
           | A_fresh _
           | A_ground _ -> ())
        c.hc_trail;
      (* register to labelled clauses *)
      List.iter
        (fun lc ->
           lc.lc_sel.select_depends <- c :: lc.lc_sel.select_depends)
        c.hc_label;
  end

let make =
  let n_ = ref 0 in
  fun ~trail ~constr ~unordered_depth ~label head body proof ->
    let hc_id = !n_ in
    incr n_;
    let c = {
      hc_id;
      hc_head=head;
      hc_unordered_depth=unordered_depth;
      hc_body=body;
      hc_proof=proof;
      hc_trail=trail;
      hc_constr=constr;
      hc_label=label;
      hc_status=(HC_dead,~-1);
    } in
    register_ c; (* register right now *)
    c

let body_seq c = IArray.to_seq (body c)
let body_l c = IArray.to_list (body c)

let body_len c = IArray.length (body c)

let body0 c =
  if IArray.length (body c) = 0
  then None
  else Some (IArray.get (body c) 0)

let body0_exn c = match body0 c with
  | Some c -> c
  | None ->
    Util.errorf ~where:"Horn_clause.body0_exn" "empty body in `@[%a@]`" pp c

let body_get c n =
  if n < 0 || n >= IArray.length (body c) then (
    Util.errorf ~where:"Horn.body_get" "%d in `@[%a@]`" n pp c;
  );
  IArray.get (body c) n

let body_tail c =
  let n = IArray.length (body c) in
  if n = 0 then Util.errorf ~where:"Horn_clause.body_tail" "empty body `@[%a@]`" pp c;
  IArray.init (n-1) (fun i -> IArray.get (body c) (i+1))

let head_pos c = PW.make (head c) Pos.(head stop)
let body_pos n c = PW.make (body_get c n) Pos.(arg n @@ body @@ stop)
let body0_pos = body_pos 0

(** {2 Helpers} *)

let is_trivial c =
  let res =
    Lit.is_trivial (head c) ||
    IArray.exists Lit.is_absurd (body c) ||
    Trail.is_absurd (trail c) ||
    Constraint.is_absurd (constr c) ||
    Label.has_no_ground_instance (label c)
  in
  if res then (
    Util.debugf 5 "(@[<2>is_trivial %a@])" (fun k->k pp c);
  );
  res

(* NOTE: some constraints will have to be solved all at once
   to obtain an actual substitution *)
let constr_are_sat (c:c_constraint): bool = not (Constraint.is_absurd c)

let is_absurd c =
  Lit.is_absurd (head c) &&
  body_len c = 0 &&
  not (Trail.is_absurd (trail c)) &&
  not (Label.has_no_ground_instance (label c)) &&
  constr_are_sat (constr c)

let is_ground c =
  Lit.is_ground (head c) &&
  IArray.for_all Lit.is_ground (body c)

let is_unit_pos c =
  not (Lit.is_absurd (head c)) &&
  IArray.length (body c) = 0

let vars_seq = Hornet_types_util.vars_of_hclause

let to_lits (c:t): Index_intf.lits =
  Sequence.cons
    (head c)
    (body c |> IArray.to_seq |> Sequence.map Lit.neg)
  |> Sequence.map Lit.to_slit

let labels (c:t): Index_intf.labels =
  trail c
  |> Trail.bool_lits
  |> Sequence.map Hornet_types_util.int_of_bool_lit
  |> Util.Int_set.of_seq

(** {2 Life Cycle} *)

(* start a new cycle, so that dead clause can be alive again *)
let start_new_cycle () : unit =
  incr cycle;
  Util.debugf 4 "@[<2>start_new_cycle (%d)@]" (fun k->k !cycle);
  ()

let current_cycle () = !cycle

(* is the clause dead right now? *)
let is_dead (c:t): bool = match status c with
  | HC_alive, _ -> false
  | HC_dead, n -> assert (n >= 0); true

let is_alive c = not (is_dead c)

let make_alive_again (c:t): unit =
  begin match status c with
    | HC_alive, _ -> ()
    | HC_dead, n ->
      assert (n<current_cycle());
      register_ c;
  end

let kill (c:t): unit =
  begin match status c with
    | HC_alive, _ ->
      (* the clause dies now *)
      Util.debugf 5 "@[<2>remove clause@ %a,@ now dead@]"
        (fun k->k pp c);
      set_status c HC_dead !cycle;
    | HC_dead, _ -> ()
  end

(** {2 Unification} *)

let prof_variant = Util.mk_profiler "hornet.horn_clause_variant"
let prof_subsume = Util.mk_profiler "horn_clause.horn_clause_subsume"
let stat_subsume_call = Util.mk_stat "horn_clause.calls_subsume"
let stat_subsume_success = Util.mk_stat "horn_clause.subsume_success"

let variant_ subst (c1,sc1) (c2,sc2) : Subst.t Sequence.t =
  let variant_constr subst (c1,sc1)(c2,sc2) =
    Constraint.variant ~subst (c1,sc1) (c2,sc2)
  in
  let {
    hc_unordered_depth=_;
    hc_body=a1;
    hc_head=h1;
    hc_constr=c1;
    hc_trail=tr1;
    hc_id=id1;
    hc_status=_;
    hc_label=lab1;
    hc_proof=_;
  } = c1
  and {
    hc_unordered_depth=_;
    hc_body=a2;
    hc_head=h2;
    hc_constr=c2;
    hc_trail=tr2;
    hc_id=id2;
    hc_label=lab2;
    hc_status=_;
    hc_proof=_;
  } = c2 in
  if id1=id2 then Sequence.return subst
  else if Hornet_types_util.equal_bool_trail tr1 tr2 then (
    Lit.variant ~subst (h1,sc1)(h2,sc2)
    |> Sequence.flat_map
      (fun subst ->
         Unif.unif_array_com subst
           (IArray.to_array_unsafe a1,sc1)
           (IArray.to_array_unsafe a2,sc2)
           ~op:(fun subst x y -> Lit.variant ~subst x y))
    |> Sequence.flat_map
      (fun subst -> variant_constr subst (c1,sc1)(c2,sc2))
    |> Sequence.flat_map
      (fun subst -> Label.variant ~subst (lab1,sc1)(lab2,sc2))
  ) else Sequence.empty

let variant ?(subst=Subst.empty) a b k =
  Util.with_prof prof_variant (fun k -> variant_ subst a b k) k

module Subsume_ = struct
  (* can [c1] reasonable subsume [c2]? *)
  let precheck (c1:t) (c2:t): bool =
    (* check that every literal in a matches at least one literal in b *)
    let all_lits_match () =
      Lit.subsumes_pred (head c1) (head c2) &&
      IArray.for_all
        (fun lita ->
           IArray.exists (fun litb -> Lit.subsumes_pred lita litb) (body c2))
        (body c1)
    (* check that every labelled clause of c1 is present in c2 *)
    and all_label_clauses_present () =
      Label.to_seq (label c1)
      |> Sequence.for_all
        (fun lc ->
           Sequence.exists
             (Labelled_clause.same_clause lc)
             (label c2 |> Label.to_seq))
    in
    IArray.length (body c1) <= IArray.length (body c2) &&
    Hornet_types_util.subsumes_bool_trail (trail c1) (trail c2) &&
    all_lits_match () &&
    all_label_clauses_present ()

  (* Compare literals by subsumption difficulty
     (see "towards efficient subsumption", Tammet).
     We sort by increasing order, so non-ground, deep, heavy literals are
     smaller (thus tested early) *)
  let compare_literals_subsumption lita litb =
    CCOrd.(
      (* ground literal is bigger *)
      bool (Lit.is_ground lita) (Lit.is_ground litb)
      (* deep literal is smaller *)
      <?> (map Lit.depth (opp int), lita, litb)
      (* heavy literal is smaller *)
      <?> (map Lit.weight (opp int), lita, litb)
    )

  let variant_constr_ subst (c1,sc1)(c2,sc2) =
    Constraint.variant ~subst (c1,sc1) (c2,sc2)

  (* Check whether [a] subsumes [b], and if it does, return the
     corresponding substitution *)
  let subsumes_with subst (a,sc_a) (b,sc_b) yield: unit =
    (* sort a copy of [a] by decreasing difficulty *)
    let a = IArray.to_array_copy a in
    let b = IArray.to_array_unsafe b in (* no modification *)
    Array.sort compare_literals_subsumption a;
    let bv = BV.empty () in
    (* try to subsumes literals of b whose index are not in bv, with [subst] *)
    let rec try_permutations i subst =
      if i = Array.length a
      then yield subst
      else (
        let lita = a.(i) in
        find_matched lita i subst 0
      )
    (* find literals of b that are not in bv and that are matched by lita *)
    and find_matched lita i subst j =
      if j = Array.length b then ()
      (* if litb is already matched, continue *)
      else if BV.get bv j then find_matched lita i subst (j+1)
      else (
        let litb = b.(j) in
        BV.set bv j;
        (* match lita and litb, then flag litb as used, and try with next literal of a *)
        let n_subst = ref 0 in
        Lit.subsumes ~subst (lita, sc_a) (litb, sc_b)
          (fun subst' ->
             incr n_subst;
             try_permutations (i+1) subst');
        BV.reset bv j;
        (* some variable of lita occur in a[j+1...], try another literal of b *)
        if !n_subst > 0 && not (check_vars lita (i+1))
        then () (* no backtracking for litb *)
        else find_matched lita i subst (j+1)
      )
    (* does some literal in a[j...] contain a variable in l or r? *)
    and check_vars lit j =
      let vars = Lit.vars_list lit in
      vars <> [] &&
      begin
        try
          for k = j to Array.length a - 1 do
            if List.exists (fun v -> Lit.var_occurs ~var:v a.(k)) vars
            then raise Exit
          done;
          false
        with Exit -> true
      end
    in
    try_permutations 0 subst

  let subsume_ subst (c1,sc1) (c2,sc2) : Subst.t Sequence.t =
    let {
      hc_unordered_depth=_;
      hc_body=a1;
      hc_head=h1;
      hc_constr=cstr1;
      hc_trail=_;
      hc_id=id1;
      hc_status=_;
      hc_label=lab1;
      hc_proof=_;
    } = c1
    and {
      hc_unordered_depth=_;
      hc_body=a2;
      hc_head=h2;
      hc_constr=cstr2;
      hc_trail=_;
      hc_id=id2;
      hc_label=lab2;
      hc_status=_;
      hc_proof=_;
    } = c2 in
    Util.incr_stat stat_subsume_call;
    if id1=id2 then Sequence.return subst
    else if precheck c1 c2 then (
      Lit.subsumes ~subst (h1,sc1)(h2,sc2)
      |> Sequence.flat_map
        (fun subst ->
           subsumes_with subst (a1,sc1) (a2,sc2))
      |> Sequence.flat_map
        (fun subst -> variant_constr_ subst (cstr1,sc1)(cstr2,sc2))
      |> Sequence.flat_map
        (fun subst -> Label.subsumes ~subst (lab1,sc1)(lab2,sc2))
      |> Sequence.map
        (fun subst -> Util.incr_stat stat_subsume_success; subst)
    ) else Sequence.empty
end

let subsumes ?(subst=Subst.empty) a b k =
  Util.with_prof prof_subsume (fun k -> Subsume_.subsume_ subst a b k) k

let subsumes_pred c1 c2 =
  not (subsumes (c1,0)(c2,1) |> Sequence.is_empty)

let equal_mod_alpha (c1:t) (c2:t) : bool =
  not (variant (c1,0)(c2,1) |> Sequence.is_empty)

let hash_mod_alpha c: int =
  Hash.combine5 42
    (Lit.hash_mod_alpha (head c))
    (IArray.hash_comm Lit.hash_mod_alpha (body c))
    (Hash.list_comm
       (fun (lazy b_lit) -> Hornet_types_util.hash_bool_lit b_lit)
       (trail c))
    (Label.hash_mod_alpha (label c))

(** {2 Containers} *)

module As_key = struct
  type t = horn_clause
  let equal = equal
  let hash = hash
  let compare = compare
end
module Tbl = CCHashtbl.Make(As_key)
module Set = CCSet.Make(As_key)

module Tbl_mod_alpha = CCHashtbl.Make(struct
    type t = horn_clause
    let equal = equal_mod_alpha
    let hash = hash_mod_alpha
  end)

(** {2 Pairing with Position} *)

module With_pos = struct
  type t = horn_clause Position.With.t
  let compare = PW.compare compare
  let pp = PW.pp pp
  let to_string = Fmt.to_string pp
end
