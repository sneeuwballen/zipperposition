
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

open Logtk

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat
module P = LLProof

type proof = LLProof.t
type term = T.t
type form = F.t

(** {2 Types} *)

type res =
  | R_ok
  | R_fail

type stats = {
  n_ok: int;
  n_fail: int;
  n_nocheck: int;
}

let section = LLProof.section
let prof_check = Util.mk_profiler "llproof_check.step"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<Green>ok@}"
  | R_fail -> Fmt.fprintf out "@{<Red>fail@}"

let pp_res_opt out = function
  | Some r -> pp_res out r
  | None -> Fmt.fprintf out "@{<Yellow>nocheck@}"

let pp_stats out (s:stats) =
  Fmt.fprintf out "(@[<hv>:ok %d@ :fail %d@ :nocheck %d@])"
    s.n_ok s.n_fail s.n_nocheck

exception Error of string

let error msg = raise (Error msg)
let errorf msg = Fmt.ksprintf ~f:error msg

let () = Printexc.register_printer
    (function
      | Error msg -> Some (Util.err_spf "llproof_check: %s" msg)
      | _ -> None)

(** {2 Tableau Prover} *)

(** A simple tableau prover for discharging every proof obligation.
    It does not do instantiation (we assume every inference step is
    preceded by relevant instantiations) but can handle renamings.
*)

module Tab : sig
  val can_check : LLProof.tag list -> bool
  (** Is this set of tags accepted by the tableau prover? *)

  val prove : form list -> form -> res
  (** [prove a b] returns [R_ok] if [a => b] is a tautology. *)
end = struct
  module T = TypedSTerm

  (** Congruence Closure *)
  module CC = Congruence.Make(struct
      include T

      let subterms t = match T.view t with
        | T.App (f, l) -> f :: l
        | T.AppBuiltin (_,l) -> l
        | T.Ite (a,b,c) -> [a;b;c]
        | T.Const _ | T.Var _
          -> []
        | T.Match _
        | T.Bind _
        | T.Let _
        | T.Multiset _
        | T.Record _
        | T.Meta _
          -> assert false (* TODO *)

      let update_subterms t l = match T.view t, l with
        | T.App (_, l1), f :: l1' when List.length l1 = List.length l1' ->
          T.app ~ty:(T.ty_exn t) f l1'
        | T.AppBuiltin (b, l1), l1' when List.length l1 = List.length l1' ->
          T.app_builtin ~ty:(T.ty_exn t) b l1'
        | (T.Const _ | T.Var _), [] -> t
        | T.Ite (_,_,_), [a;b;c] -> T.ite a b c
        | T.App _, _
        | T.AppBuiltin _, _
        | T.Const _, _
        | T.Var _, _
        | T.Ite _, _
        | T.Match _, _
        | T.Bind _, _
        | T.Let _, _
        | T.Multiset _, _
        | T.Record _, _
        | T.Meta _, _ -> assert false
    end)

  (** Branches of the tableau. A branch is a conjunction of formulas
      plus some theory context (congruence closure).
      A branch is closed if it's inconsistent *)
  module Branch : sig
    type t

    val make : form list -> t

    val closed : t -> bool

    val add : t -> form list -> t
    (** add the given set of formulas *)

    val pop_open : t -> (form * t) option
    (** remove and return next formula to expand *)

    val debug : t Fmt.printer
  end = struct
    type t = {
      expanded: form list;
      to_expand : form list;
      cc: CC.t;
      diseq: (term * term) list; (* negative constraints *)
      closed: bool;
    }

    (* make a new empty branch *)
    let empty () = {
      expanded=[];
      to_expand=[];
      cc=CC.create();
      diseq=[(F.true_, F.false_)];
      closed=false;
    }

    (* check if some diseq is true *)
    let check_closed (b:t) : t =
      if b.closed then b
      else (
        let closed = List.exists (fun (t,u) -> CC.is_eq b.cc t u) b.diseq in
        if closed then {b with closed} else b
      )

    let[@inline] add_cc_eq t u b = { b with cc=CC.mk_eq b.cc t u }
    let[@inline] add_diseq_ t u b = { b with diseq=(t,u)::b.diseq }

    let[@inline] add_expanded f b = {b with expanded = f::b.expanded}
    let[@inline] add_eq t u b : t = b |> add_expanded (F.eq t u) |> add_cc_eq t u |> check_closed
    let[@inline] add_diseq t u b : t = b |> add_diseq_ t u |> check_closed
    let[@inline] add_to_expand f b = {b with to_expand = f::b.to_expand}

    (* add one formula to [b] *)
    let add1 (br:t) (f:form): t =
      begin match F.view f with
        | F.Atom t -> add_eq t F.true_ br
        | F.True -> br
        | F.False -> add_eq F.true_ F.false_ br
        | F.Eq (t,u) -> add_eq t u br
        | F.Neq (t,u) -> add_diseq t u br
        | F.Not f' ->
          begin match F.view f' with
            | F.Eq _ | F.Neq _ | F.Not _ -> assert false
            | F.True -> add_diseq F.true_ F.false_ br
            | F.False -> br
            | F.Atom t -> add_eq t F.false_ br
            | F.And l -> add_to_expand (F.or_ (List.map F.not_ l)) br
            | F.Or l -> add_to_expand (F.and_ (List.map F.not_ l)) br
            | F.Imply (a,b) -> add_to_expand (F.and_ [a; F.not_ b]) br
            | F.Equiv (a,b) ->
              add_to_expand (F.or_ [F.and_ [a; F.not_ b]; F.and_ [b; F.not_ a]]) br
            | F.Xor (a,b) ->
              add_to_expand (F.and_ [F.or_ [a; F.not_ b]; F.or_ [b; F.not_ a]]) br
            | F.Forall (v,body) ->
              add_expanded (F.exists v (F.not_ body)) br (* TODO? *)
            | F.Exists (v,body) ->
              add_expanded (F.forall v (F.not_ body)) br (* TODO? *)
          end
        | F.And _ | F.Or _ -> add_to_expand f br
        | F.Imply (a,b) -> add_to_expand (F.or_ [F.not_ a; b]) br
        | F.Xor (a,b) ->
          add_to_expand (F.or_ [F.and_ [a; F.not_ b]; F.and_ [b; F.not_ a]]) br
        | F.Equiv (a,b) ->
          add_to_expand (F.and_ [F.or_ [a; F.not_ b]; F.or_ [b; F.not_ a]]) br
        | F.Forall (v,body) ->
          add_expanded (F.exists v (F.not_ body)) br (* TODO? *)
        | F.Exists (v,body) ->
          add_expanded (F.forall v (F.not_ body)) br (* TODO? *)
      end

    let[@inline] add b l = List.fold_left add1 b l
    let[@inline] make l = add (empty ()) l
    let[@inline] closed b = b.closed

    let pop_open b =
      if closed b then None
      else begin match b.to_expand with
        | [] -> None
        | f :: tail -> Some (f, {b with to_expand=tail; expanded=f::b.expanded})
      end

    let debug out (b:t) : unit =
      Fmt.fprintf out
        "(@[<hv>branch (closed %B)@ \
         :to_expand (@[<hv>%a@])@ :expanded (@[<hv>%a@])@])"
        b.closed (Util.pp_list T.pp) b.to_expand (Util.pp_list T.pp) b.expanded
  end

  (** Rules for the Tableau calculus *)
  module Rule : sig
    type t

    val apply : t list -> form -> form list list
    (** Return a disjunctive list of conjuctions *)

    val all : t list
  end = struct
    type t = form -> form list list

    let or_ f = match F.view f with
      | F.Or l -> List.map CCList.return l
      | _ -> []

    let and_ f = match F.view f with
      | F.And l -> [l]
      | _ -> []

    let all = [
      or_;
      and_;
    ]

    let[@inline] apply (l:t list) (f:form) : form list list =
      CCList.flat_map (fun r -> r f) l
  end

  type branch = Branch.t

  (** main state *)
  type t = {
    mutable open_branches: branch list;
    mutable closed_branches: branch list;
  }

  let debug_tag out (tab:t) : unit =
    Fmt.fprintf out "(@[<v>%a@])"
      (Util.pp_list Branch.debug)
      (List.rev_append tab.open_branches tab.closed_branches)

  exception Saturated_branch of branch

  (* solve tableau by expanding it piece by piece *)
  let solve_ (tab:t) : res =
    try
      while tab.open_branches <> [] do
        let b = List.hd tab.open_branches in
        tab.open_branches <- List.tl tab.open_branches;
        Util.debugf ~section 3
          "(@[llproof_check.tab.solve@ %a@])"
          (fun k->k debug_tag tab);
        begin match Branch.pop_open b with
          | None ->
            if Branch.closed b then (
              tab.closed_branches <- b :: tab.closed_branches
            ) else (
              (* cannot close this branch *)
              raise (Saturated_branch b)
            )
            (* saturated *)
          | Some (f, b_tail) ->
            let new_branches =
              Rule.apply Rule.all f
              |> List.map
                (fun forms -> Branch.add b_tail forms)
            in
            (* add new branches *)
            List.iter
              (fun b ->
                 if Branch.closed b then (
                   tab.closed_branches <- b :: tab.closed_branches
                 ) else  (
                   tab.open_branches <- b :: tab.open_branches
                 ))
              new_branches
        end
      done;
      (* closed all branches *)
      assert (tab.open_branches=[]);
      Util.debugf ~section 5 "(@[proof.tableau.success@ :branches (@[<v>%a@])@])"
        (fun k->k (Util.pp_list Branch.debug) tab.closed_branches);
      R_ok
    with Saturated_branch b ->
      (* found a branch that is not refutable *)
      Util.debugf ~section 1 "(@[proof.tableau.failed@ :branch %a@])"
        (fun k->k Branch.debug b);
      R_fail

  let can_check : LLProof.tag list -> bool =
    let module P = Proof in
    let f = function P.T_lra | P.T_lia | P.T_ho | P.T_ind | P.T_data -> false in
    List.for_all f

  let prove (a:form list) (b:form) =
    Util.debugf ~section 3
      "(@[llproof_checl.tab.prove@ :hyps (@[<hv>%a@])@ :concl %a@])"
      (fun k->k (Util.pp_list T.pp) a T.pp b);
    let b_init = Branch.make (F.not_ b :: a) in
    let tab = {
      open_branches=[b_init];
      closed_branches=[];
    } in
    solve_ tab
end

(** {2 Checking Proofs} *)

(* TODO: have another global graph structure with inference steps, ESA,
   instantiation steps? *)

let instantiate (f:form) (inst:LLProof.inst) : form =
  let vars, body = T.unfold_binder Binder.Forall f in
  if List.length vars <> List.length inst then (
    errorf "mismatched arities in instantiate `%a`@ :with %a"
      T.pp f LLProof.pp_inst inst
  );
  let subst =
    List.fold_left2 Var.Subst.add Var.Subst.empty vars inst
  in
  let f' = T.Subst.eval_nonrec subst body in
  Util.debugf ~section 5
    "(@[<hv>instantiate@ :inst %a@ :from %a@ :into %a@])"
    (fun k->k LLProof.pp_inst inst T.pp f T.pp f');
  f'

let rename (f:form) (rn:LLProof.renaming) : form =
  let vars, body = T.unfold_binder Binder.Forall f in
  if List.length vars <> List.length rn then (
    errorf "mismatched arities in rename `%a`@ :with %a"
      T.pp f LLProof.pp_renaming rn
  );
  let subst =
    List.fold_left2 Var.Subst.add Var.Subst.empty vars rn
  in
  let f' = T.rename subst body in
  Util.debugf ~section 5
    "(@[<hv>instantiate@ :inst %a@ :from %a@ :into %a@])"
    (fun k->k LLProof.pp_renaming rn T.pp f T.pp f');
  f'

let concl_of_parent (p:LLProof.parent) : form = match p.LLProof.p_rename with
  | [] -> P.concl p.LLProof.p_proof
  | r ->
    let f = P.concl p.LLProof.p_proof in
    rename f r

let check_step_ (p:proof): res option =
  let concl = P.concl p in
  begin match P.step p with
    | P.Goal
    | P.Assert
    | P.By_def _
    | P.Define _
      -> Some R_ok
    | P.Negated_goal p' ->
      (* [p'] should prove [not concl] *)
      Some (Tab.prove [P.concl p'] (F.not_ concl))
    | P.Trivial ->
      (* should be able to prove the conclusion directly *)
      Some (Tab.prove [] concl)
    | P.Instantiate (_,_) -> None (* TODO *)
    | P.Esa (_,_,_) -> None (* TODO *)
    | P.Inference {check=P.C_other;_} -> Some R_ok
    | P.Inference {check=P.C_no_check;_} -> None
    | P.Inference {parents;check=P.C_check axioms;tags;intros;_} ->
      if Tab.can_check tags then (
        (* within the fragment of {!Tab.prove} *)
        let all_premises =
          axioms @ List.map concl_of_parent parents
        and concl =
          rename concl intros
        in
        Some (Tab.prove all_premises concl)
      ) else None
  end

let check_step p = Util.with_prof prof_check check_step_ p

let check
    ?(before_check=fun _ -> ())
    ?(on_check=fun _ _ -> ())
    (p:proof) : res * stats
  =
  let tbl = P.Tbl.create 64 in
  let stats = ref {n_ok=0; n_fail=0; n_nocheck=0} in
  let upd_stats f = stats := f !stats in
  let rec check (p:proof): unit =
    if not (P.Tbl.mem tbl p) then (
      before_check p;
      Util.debugf ~section 3 "(@[@{<Yellow>start_checking_proof@}@ %a@])" (fun k->k P.pp p);
      let res = check_step p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3
        "(@[<hv>@{<Yellow>done_checking_proof@}@ :of %a@ :res %a@])"
        (fun k->k P.pp p pp_res_opt res);
      on_check p res;
      begin match res with
        | Some R_ok -> upd_stats (fun s -> {s with n_ok = s.n_ok+1})
        | Some R_fail -> upd_stats (fun s -> {s with n_fail = s.n_fail+1})
        | None -> upd_stats (fun s -> {s with n_nocheck = s.n_nocheck+1})
      end;
      (* now check premises *)
      List.iter check (P.premises p)
    );
  in
  check p;
  if !stats.n_fail = 0 then R_ok, !stats else R_fail, !stats
