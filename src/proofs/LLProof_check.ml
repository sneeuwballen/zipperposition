
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Check LLProof} *)

open Logtk

module T = TypedSTerm
module F = T.Form
module Ty = T.Ty
module Fmt = CCFormat
module P = LLProof

type proof = LLProof.t
type form = F.t

(** {2 Types} *)

type res =
  | R_ok
  | R_fail

type stats = {
  n_ok: int;
  n_fail: int;
  n_skip_esa: int; (** steps skipped because ESA *)
  n_skip_tags: int; (** steps skipped because of theory tags *)
  n_skip_trivial: int; (** steps skipped because they are trivial *)
  n_skip: int;
}

let section = LLProof.section
let prof_check = Util.mk_profiler "llproof.check.step"
let stat_check = Util.mk_stat "llproof.check.step"
let stat_tab_solve = Util.mk_stat "llproof.check.tab_solve"

let pp_res out = function
  | R_ok -> Fmt.fprintf out "@{<Green>OK@}"
  | R_fail -> Fmt.fprintf out "@{<Red>FAIL@}"

let pp_stats out (s:stats) =
  Fmt.fprintf out "(@[<hv>:ok %d@ :fail %d@ :skip %d (:esa %d@ :tags %d)@])"
    s.n_ok s.n_fail s.n_skip s.n_skip_esa s.n_skip_tags

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
  module T = LLTerm
  module F = LLTerm.Form

  (** Congruence Closure *)
  module CC = Congruence.Make(struct
      include T

      let subterms t = match T.view t with
        | T.App (f, a) -> [f;a]
        | T.Arrow (a,b) -> [a;b]
        | T.AppBuiltin (Builtin.Box_opaque, _) -> []  (* simple equality *)
        | T.AppBuiltin (_,l) -> l
        | T.Ite (a,b,c) -> [a;b;c]
        | T.Bind {body;_} -> [body]
        | T.Const _ | T.Var _ | T.Type
          -> []

      let update_subterms t l = match T.view t, l with
        | T.App (_, _), [f;a] -> T.app f a
        | T.Arrow (_, _), [a;b] -> T.arrow a b
        | T.AppBuiltin (b, l1), l1' when List.length l1 = List.length l1' ->
          T.app_builtin ~ty:(T.ty_exn t) b l1'
        | T.Bind {binder;ty_var;_}, [body] ->
          T.bind ~ty:(T.ty_exn t) binder ~ty_var body
        | (T.Const _ | T.Var _), [] -> t
        | T.Ite (_,_,_), [a;b;c] -> T.ite a b c
        | T.App _, _
        | T.Arrow _, _
        | T.AppBuiltin _, _
        | T.Bind _, _
        | T.Const _, _
        | T.Var _, _
        | T.Type, _
        | T.Ite _, _
          -> assert false
    end)

  (** Branches of the tableau. A branch is a conjunction of formulas
      plus some theory context (congruence closure).
      A branch is closed if it's inconsistent *)
  module Branch : sig
    type t

    val make : T.t list -> t

    val closed : t -> bool

    val add : t -> T.t list -> t
    (** add the given set of formulas *)

    val pop_open : t -> (T.t * t) option
    (** remove and return next formula to expand *)

    val debug : t Fmt.printer
  end = struct
    module T_set = T.Set

    type t = {
      expanded: T_set.t;
      to_expand : T_set.t;
      cc: CC.t;
      diseq: (T.t * T.t) list; (* negative constraints *)
      closed: bool;
    }

    (* make a new empty branch *)
    let empty () = {
      expanded=T_set.empty;
      to_expand=T_set.empty;
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

    let[@inline] add_cc_ t b = { b with cc=CC.add b.cc t }
    let[@inline] add_cc_eq t u b = { b with cc=CC.mk_eq b.cc t u }
    let[@inline] add_diseq_ t u b = { b with diseq=(t,u)::b.diseq }

    let[@inline] add_expanded f b = {b with expanded = T_set.add f b.expanded}
    let[@inline] add_eq t u b : t = b |> add_expanded (F.eq t u) |> add_cc_eq t u |> check_closed
    let[@inline] add_diseq t u b : t = b |> add_diseq_ t u |> check_closed
    let[@inline] add_to_expand f b = {b with to_expand = T_set.add f b.to_expand}

    let[@inline] add_form_to_expand f b =
      b |> add_to_expand f |> check_closed

    let rec is_atomic f = match F.view f with
      | F.Eq _ | F.Neq _ | F.Atom _ -> true
      | F.Not u -> is_atomic u
      | _ -> false

    (* add one formula to [b] *)
    let add1 (br:t) (f:T.t): t =
      let br = add_cc_ f br in
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
            | F.And l -> add_form_to_expand (F.or_ (List.map F.not_ l)) br
            | F.Or l -> add_form_to_expand (F.and_ (List.map F.not_ l)) br
            | F.Imply (a,b) -> add_form_to_expand (F.and_ [a; F.not_ b]) br
            | F.Equiv (a,b) ->
              add_form_to_expand (F.or_ [F.and_ [a; F.not_ b]; F.and_ [b; F.not_ a]]) br
            | F.Xor (a,b) ->
              add_form_to_expand (F.and_ [F.or_ [a; F.not_ b]; F.or_ [b; F.not_ a]]) br
            | F.Exists {ty_var;body}  ->
              let f = F.forall ~ty_var (F.not_ body) in
              br |> add_expanded f |> add_cc_eq f F.true_ |> check_closed
            | F.Forall _ ->
              br |> add_expanded f |> add_cc_eq f F.false_ |> check_closed
          end
        | F.And _ | F.Or _ -> add_form_to_expand f br
        | F.Imply (a,b) -> add_form_to_expand (F.or_ [F.not_ a; b]) br
        | F.Xor (a,b) ->
          add_form_to_expand (F.or_ [F.and_ [a; F.not_ b]; F.and_ [b; F.not_ a]]) br
        | F.Equiv (a,b) ->
          add_form_to_expand (F.and_ [F.or_ [a; F.not_ b]; F.or_ [b; F.not_ a]]) br
        | F.Forall _ ->
          br |> add_expanded f |> add_cc_eq f F.true_ |> check_closed
        | F.Exists {ty_var;body} ->
          let f = F.forall ~ty_var (F.not_ body) in
          br |> add_expanded f |> add_cc_eq f F.false_ |> check_closed
      end

    let[@inline] add b l =
      (* put atomic formulas first *)
      let l =
        List.sort
          (fun a b -> match is_atomic a, is_atomic b with
             | true, true | false, false -> 0
             | true, false -> -1 | false, true -> 1)
          l
      in
      List.fold_left add1 b l

    let[@inline] make l = add (empty ()) l
    let[@inline] closed b = b.closed

    let pop_open b =
      if closed b then None
      else begin match T_set.choose b.to_expand with
        | f ->
          let tail = T_set.remove f b.to_expand in
          Some (f, {b with to_expand=tail; expanded=T_set.add f b.expanded})
        | exception Not_found -> None
      end

    let debug out (b:t) : unit =
      Fmt.fprintf out
        "(@[<hv>branch (closed %B)@ \
         :to_expand (@[<hv>%a@])@ :expanded (@[<hv>%a@])@])"
        b.closed
        (Util.pp_seq T.pp) (T_set.to_seq b.to_expand)
        (Util.pp_seq T.pp) (T_set.to_seq b.expanded)
  end

  (** Rules for the Tableau calculus *)
  module Rule : sig
    type t

    val apply : t list -> F.t -> F.t list list
    (** Return a disjunctive list of conjuctions *)

    val all : t list
  end = struct
    type t = F.t -> F.t list list

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

    let[@inline] apply (l:t list) (f:F.t) : F.t list list =
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
          "(@[llproof.check.tab.solve@ %a@])"
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
      Util.debugf ~section 5 "(@[llproof.check.tab.success@ :branches (@[<v>%a@])@])"
        (fun k->k (Util.pp_list Branch.debug) tab.closed_branches);
      R_ok
    with Saturated_branch b ->
      (* found a branch that is not refutable *)
      Util.debugf ~section 1 "(@[llproof.check.tab.failed@ :branch %a@])"
        (fun k->k Branch.debug b);
      R_fail

  let can_check : LLProof.tag list -> bool =
    let open Builtin.Tag in
    let f = function
      | T_ho -> true
      | T_lra | T_lia | T_ind | T_data
      | T_distinct | T_ac _ | T_ext -> false
    in
    List.for_all f

  let prove (a:form list) (b:form) =
    Util.debugf ~section 3
      "(@[@{<yellow>llproof.check.tab.prove@}@ :hyps (@[<hv>%a@])@ :concl %a@])"
      (fun k->k (Util.pp_list TypedSTerm.pp) a TypedSTerm.pp b);
    Util.incr_stat stat_tab_solve;
    (* convert into {!LLTerm.t} *)
    let ctx = T.Conv.create() in
    let a = List.map (T.Conv.of_term ctx) a in
    let b = T.Conv.of_term ctx b in
    (* prove [a ∧ -b ⇒ ⊥] *)
    let b_init = Branch.make (F.not_ b :: a) in
    let tab = {
      open_branches=[b_init];
      closed_branches=[];
    } in
    solve_ tab
end

(** {2 Checking Proofs} *)

let instantiate (f:form) (inst:LLProof.inst) : form =
  let f = T.rename_all_vars f in
  let vars, body = T.unfold_binder Binder.Forall f in
  if List.length vars <> List.length inst then (
    errorf "mismatched arities in instantiate `%a`@ :with %a"
      T.pp f LLProof.pp_inst inst
  );
  let subst =
    List.fold_left2 Var.Subst.add Var.Subst.empty vars inst
  in
  let f' = T.Subst.eval subst body in
  Util.debugf ~section 5
    "(@[<hv>instantiate@ :inst %a@ :from %a@ :into %a@ :subst {%a}@])"
    (fun k->k LLProof.pp_inst inst T.pp f T.pp f' (Var.Subst.pp T.pp_with_ty) subst);
  f'

let concl_of_parent (p:LLProof.parent) : form = match p.LLProof.p_inst with
  | [] -> P.concl p.LLProof.p_proof
  | r ->
    let f = P.concl p.LLProof.p_proof in
    instantiate f r

let open_forall = T.unfold_binder Binder.Forall

type check_step_res =
  | CS_check of res
  | CS_skip of [`ESA | `Other | `Tags | `Trivial]

let pp_csr out = function
  | CS_check r -> pp_res out r
  | CS_skip r ->
    let s = match r with
      | `ESA -> "esa" | `Tags -> "tags"
      | `Other -> "other" | `Trivial -> "trivial" in
    Fmt.fprintf out "@{<Yellow>SKIP@} (%s)" s

let check_step_ (p:proof): check_step_res =
  let concl = P.concl p in
  Util.incr_stat stat_check;
  begin match P.step p with
    | P.Goal
    | P.Assert
    | P.By_def _
    | P.Define _
      -> CS_check R_ok
    | P.Negated_goal p' ->
      (* [p'] should prove [not concl] *)
      CS_check (Tab.prove [P.concl p'] (F.not_ concl))
    | P.Trivial -> CS_skip `Other (* axiom of the theory *)
    | P.Instantiate {tags;_} when not (Tab.can_check tags) -> CS_skip `Tags
    | P.Instantiate {form=p';inst;_} ->
      (* re-instantiate and check we get the same thing *)
      let p'_inst = instantiate (LLProof.concl p') inst in
      let vars, body_concl = open_forall concl in
      (* now remove free variables by using fresh constants *)
      let subst =
        vars
        |> List.mapi (fun i v -> v, T.const ~ty:(Var.ty v) (ID.makef "sk_%d" i))
        |> Var.Subst.of_list
      in
      CS_check (Tab.prove [T.Subst.eval subst p'_inst] (T.Subst.eval subst body_concl))
    | P.Esa (_,_) -> CS_skip `ESA (* TODO *)
    | P.Inference {parents;tags;intros;_} ->
      if Tab.can_check tags then (
        (* within the fragment of {!Tab.prove} *)
        let all_premises =
          List.map concl_of_parent parents
        and concl =
          instantiate concl intros
        in
        CS_check (Tab.prove all_premises concl)
      ) else CS_skip `Tags
  end

let check_step p = Util.with_prof prof_check check_step_ p

let check
    ?(before_check=fun _ -> ())
    ?(on_check=fun _ _ -> ())
    (p:proof) : res * stats
  =
  let tbl = P.Tbl.create 64 in
  let stats = ref {
      n_ok=0; n_fail=0; n_skip_esa=0; n_skip_tags=0;
      n_skip_trivial=0; n_skip=0;
    } in
  let upd_stats f = stats := f !stats in
  let rec check (p:proof): unit =
    if not (P.Tbl.mem tbl p) then (
      before_check p;
      Util.debugf ~section 3 "(@[@{<Yellow>start_checking_proof@}@ %a@])"
        (fun k->k P.pp p);
      let res = check_step p in
      P.Tbl.add tbl p res;
      Util.debugf ~section 3
        "(@[<hv>@{<Yellow>done_checking_proof@}@ :of %a@ :res %a@])"
        (fun k->k P.pp p pp_csr res);
      on_check p res;
      begin match res with
        | CS_check R_ok -> upd_stats (fun s -> {s with n_ok = s.n_ok+1})
        | CS_check R_fail -> upd_stats (fun s -> {s with n_fail = s.n_fail+1})
        | CS_skip r ->
          upd_stats
            (fun s ->
               {s with
                  n_skip = s.n_skip+1;
                  n_skip_esa=if r=`ESA then s.n_skip_esa+1 else s.n_skip_esa;
                  n_skip_tags=if r=`Tags then s.n_skip_tags+1 else s.n_skip_tags;
                  n_skip_trivial=if r=`Trivial then s.n_skip_trivial+1 else s.n_skip_trivial;
               })
      end;
      (* now check premises *)
      List.iter check (P.premises p)
    );
  in
  check p;
  if !stats.n_fail = 0 then R_ok, !stats else R_fail, !stats
