
(** {1 Low Level Prover} *)

open Logtk

module T = LLTerm
module F = LLTerm.Form
module Fmt = CCFormat

type form = LLTerm.Form.t

type res =
  | R_ok
  | R_fail

let section = LLProof.section
let stat_solve = Util.mk_stat "llprover.prove"

(** Congruence Closure *)
module CC = Congruence.Make(struct
    include T
    let pp = pp_inner

    let subterms t = match T.view t with
      | T.App (f, a) -> [f;a]
      | T.Arrow (a,b) -> [a;b]
      | T.AppBuiltin (Builtin.Box_opaque, _) -> []  (* simple equality *)
      | T.AppBuiltin (_,l) -> l
      | T.Ite (a,b,c) -> [a;b;c]
      | T.Bind {body;_} -> [body]
      | Int_pred (l,_) -> T.Linexp_int.subterms l |> Sequence.to_list
      | Rat_pred (l,_) -> T.Linexp_rat.subterms l |> Sequence.to_list
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
      | Int_pred (le,op), l ->
        let l' = T.Linexp_int.subterms le |> Sequence.to_list in
        assert (List.length l = List.length l');
        let map = List.combine l' l in
        let le' = T.Linexp_int.map (fun t -> CCList.Assoc.get_exn ~eq:T.equal t map) le in
        T.int_pred le' op
      | Rat_pred (le,op), l ->
        let l' = T.Linexp_rat.subterms le |> Sequence.to_list in
        assert (List.length l = List.length l');
        let map = List.combine l' l in
        let le' = T.Linexp_rat.map (fun t -> CCList.Assoc.get_exn ~eq:T.equal t map) le in
        T.rat_pred le' op
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

  val root : unit -> t

  val check_closed : t -> t
  val is_closed : t -> bool

  val add : t -> T.t list -> t
  (** add the given set of formulas *)

  val pop_to_expand : t -> (T.t * t) option
  (** remove and return next formula to expand *)

  type closed =
    | C_not_closed
    | C_closed_by_diseq of T.t * T.t
    | C_closed_by_theory of string

  val to_expand : t -> T.t Sequence.t
  val form : t -> F.t option
  val closed : t -> closed
  val id : t -> int
  val parent : t -> t option
  val diseq : t -> (T.t * T.t) Sequence.t

  val debug : t CCFormat.printer
end = struct
  module T_set = T.Set

  type closed =
    | C_not_closed
    | C_closed_by_diseq of T.t * T.t
    | C_closed_by_theory of string

  type t = {
    id: int; (* generative ID *)
    form: T.t option;
    to_expand : T_set.t; (* all to expand *)
    cc: CC.t;
    diseq: (T.t * T.t) list; (* negative constraints *)
    mutable closed: closed;
    parent: t option;
  }

  let[@inline] closed t = t.closed
  let[@inline] to_expand t = T_set.to_seq t.to_expand
  let[@inline] parent t = t.parent
  let[@inline] id t = t.id
  let[@inline] diseq t = Sequence.of_list t.diseq
  let[@inline] form t = t.form

  let root () : t = {
    id=0;
    form=None;
    to_expand=T_set.empty;
    cc=CC.create();
    diseq=[(F.true_, F.false_)];
    closed=C_not_closed;
    parent=None;
  }

  let[@inline] is_closed b : bool = match b.closed with
    | C_not_closed -> false
    | _ -> true

  let mk_child =
    let n = ref 1 in
    let aux (f:F.t) (b:t) : t =
      { b with id=CCRef.incr_then_get n; form=Some f; parent=Some b }
    in
    aux

  (* check if some diseq is true *)
  let check_closed (b:t) : t =
    if is_closed b then b
    else (
      begin match CCList.find_pred (fun (t,u) -> CC.is_eq b.cc t u) b.diseq with
        | None -> ()
        | Some (t,u) -> b.closed <- C_closed_by_diseq (t,u)
      end;
      b
    )

  let[@inline] add_cc_ t b = { b with cc=CC.add b.cc t; }
  let[@inline] add_cc_eq t u b = { b with cc=CC.mk_eq b.cc t u }
  let[@inline] add_diseq_l_ t u b = { b with diseq=(t,u)::b.diseq }

  let[@inline] add_eq t u b : t =
    if is_closed b then b else b |> add_cc_eq t u |> check_closed
  let[@inline] add_diseq t u b : t =
    if is_closed b then b else b |> add_cc_ t |> add_cc_ u |> add_diseq_l_ t u |> check_closed
  let[@inline] add_to_expand f b =
    if is_closed b then b else {b with to_expand=T_set.add f b.to_expand}

  let[@inline] add_form_to_expand f b =
    b |> add_to_expand f |> check_closed

  let rec is_atomic f = match F.view f with
    | F.Eq _ | F.Neq _ | F.Atom _ | F.Int_pred _ | F.Rat_pred _ -> true
    | F.Not u -> is_atomic u
    | _ -> false

  (* add one formula to [b] *)
  let add1_ (br:t) (f:T.t): t =
    let br = mk_child f @@ add_cc_ f br in
    begin match F.view f with
      | F.Atom t -> add_eq t F.true_ br
      | F.True -> br
      | F.False -> add_eq F.true_ F.false_ br
      | F.Eq (t,u) -> add_eq t u br
      | F.Neq (t,u) -> add_diseq t u br
      | F.Int_pred (_,_) -> add_eq f F.true_ br (* TODO: decision proc *)
      | F.Rat_pred (_,_) -> add_eq f F.true_ br (* TODO: simplex *)
      | F.Not f' ->
        begin match F.view f' with
          | F.Eq _ | F.Neq _ | F.Not _ | F.Int_pred _ | F.Rat_pred _ -> assert false
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
            br |> add_eq f F.true_
          | F.Forall _ ->
            br |> add_eq f F.false_
        end
      | F.And _ | F.Or _ -> add_form_to_expand f br
      | F.Imply (a,b) -> add_form_to_expand (F.or_ [F.not_ a; b]) br
      | F.Xor (a,b) ->
        add_form_to_expand (F.or_ [F.and_ [a; F.not_ b]; F.and_ [b; F.not_ a]]) br
      | F.Equiv (a,b) ->
        add_form_to_expand (F.and_ [F.or_ [a; F.not_ b]; F.or_ [b; F.not_ a]]) br
      | F.Forall _ ->
        br |> add_eq f F.true_
      | F.Exists {ty_var;body} ->
        let f = F.forall ~ty_var (F.not_ body) in
        br |> add_eq f F.false_
    end

  let add1 br f : t = if is_closed br then br else add1_ br f

  let add b l =
    (* put atomic formulas first *)
    let l =
      List.sort
        (fun a b -> match is_atomic a, is_atomic b with
           | true, true | false, false -> 0
           | true, false -> -1 | false, true -> 1)
        l
    in
    List.fold_left add1 b l

  let pop_to_expand b =
    if is_closed b then None
    else begin match T_set.choose b.to_expand with
      | f ->
        let tail = T_set.remove f b.to_expand in
        Some (f, {b with to_expand=tail;})
      | exception Not_found -> None
    end

  let rec unfold_parents b = match b.parent with
    | None -> [b]
    | Some p -> b :: unfold_parents p

  let debug out (b:t) : unit =
    (* print one branch *)
    let pp_b out b =
      Fmt.fprintf out
        "(@[<hv>branch/%d :closed %B@ :form (%a)@ \
         :to_expand (@[<hv>%a@])@])"
        b.id (is_closed b)
        (Fmt.some T.pp) b.form
        (Util.pp_seq T.pp) (T_set.to_seq b.to_expand)
    in
    Fmt.fprintf out "(@[<v>%a@])" (Util.pp_list pp_b) (unfold_parents b)
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
  mutable saturated: branch option;
}

type final_state = t

let debug_tab out (tab:t) : unit =
  Fmt.fprintf out "(@[tab@ :branches (@[<v>%a@])@ :saturated (%a)@])"
    (Util.pp_list Branch.debug)
    (List.rev_append tab.open_branches tab.closed_branches)
    (Fmt.some Branch.debug) tab.saturated

(* solve tableau by expanding it piece by piece *)
let solve_ (tab:t) : res =
  while not (CCList.is_empty tab.open_branches) && CCOpt.is_none tab.saturated do
    let b = List.hd tab.open_branches in
    tab.open_branches <- List.tl tab.open_branches;
    Util.debugf ~section 3
      "(@[llproof.check.tab.solve@ %a@])" (fun k->k debug_tab tab);
    begin match Branch.pop_to_expand b with
      | None ->
        if Branch.is_closed b then (
          tab.closed_branches <- b :: tab.closed_branches
        ) else (
          (* cannot close this branch, it has no form to expand *)
          tab.saturated <- Some b;
        )
      | Some (f, b_tail) ->
        let new_branches =
          Rule.apply Rule.all f
          |> List.map
            (fun forms -> Branch.add b_tail forms)
        in
        assert (not @@ CCList.is_empty new_branches);
        (* add new branches *)
        List.iter
          (fun b ->
             if Branch.is_closed b then (
               tab.closed_branches <- b :: tab.closed_branches
             ) else  (
               tab.open_branches <- b :: tab.open_branches
             ))
          new_branches;
    end
  done;
  (* closed all branches, or found saturation *)
  begin match tab.saturated with
    | Some b ->
      (* found a branch that is not refutable *)
      assert (not (Branch.is_closed @@ Branch.check_closed b));
      Util.debugf ~section 1 "(@[llprover.prove.failed@ :branch %a@])"
        (fun k->k Branch.debug b);
      R_fail
    | None ->
      assert (CCList.is_empty tab.open_branches);
      Util.debugf ~section 5
        "(@[llprover.prove.success@ :branches (@[<v>%a@])@ :saturated %a@])"
        (fun k->k (Util.pp_list Branch.debug) tab.closed_branches
            (Fmt.some Branch.debug) tab.saturated);
      R_ok
  end

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
    "(@[@{<yellow>llprover.prove@}@ :hyps (@[<hv>%a@])@ :concl %a@])"
    (fun k->k (Util.pp_list T.pp) a T.pp b);
  Util.incr_stat stat_solve;
  (* prove [a ∧ -b ⇒ ⊥] *)
  let b_init = Branch.add (Branch.root()) (F.not_ b :: a) in
  let tab = {
    open_branches=[b_init];
    closed_branches=[];
    saturated=None;
  } in
  solve_ tab, tab

let pp_stats out (s:final_state) =
  let n_open = List.length s.open_branches in
  let n_closed = List.length s.closed_branches in
  Format.fprintf out "(@[llprover.stats@ :n_branches %d@ :n_closed %d@])"
    (n_closed + n_open) n_closed

let _to_str_escape fmt =
  Util.ksprintf_noc ~f:Util.escape_dot fmt

let pp_dot out (s:final_state) : unit =
  let module ISet = Util.Int_set in
  let as_graph =
    CCGraph.make
      (fun b -> Branch.parent b |> Sequence.of_opt |> Sequence.map (fun v->(),v))
  in
  let saturated_set =
    Sequence.of_opt s.saturated |>
    Sequence.fold (fun s b -> ISet.add (Branch.id b) s) ISet.empty
  in
  let br_eq b1 b2 = CCInt.equal (Branch.id b1) (Branch.id b2) in
  let br_hash b = Hash.int @@ Branch.id b in
  let tbl = CCGraph.mk_table ~eq:br_eq ~hash:br_hash 32 in
  let attrs_v (b:Branch.t) : _ list =
    let color =
      if Branch.is_closed b then [`Color "green"]
      else if ISet.mem (Branch.id b) saturated_set then [`Color "red"]
      else []
    in
    let pp_closed out b = match Branch.closed b with
      | Branch.C_not_closed ->
        Fmt.fprintf out "<not closed> (%d to expand)" (Branch.to_expand b |> Sequence.length)
      | Branch.C_closed_by_diseq (t,u) ->
        Fmt.fprintf out "<closed by `@[<1>%a ≠@ %a@]`>" T.pp t T.pp u
      | Branch.C_closed_by_theory s ->
        Fmt.fprintf out "<closed by theory %S>" s
    in
    let label =
      _to_str_escape "@[<v>[%d] %a@ (@[%a@])@]"
        (Branch.id b) pp_closed b
        (Fmt.some T.pp) (Branch.form b)
    in
    [`Label label; `Shape "box"; `Style "filled"] @ color
  in
  let all_branches =
    Sequence.append
      (Sequence.of_list s.open_branches)
      (Sequence.of_list s.closed_branches)
  in
  CCGraph.Dot.pp_seq
    ~tbl
    ~eq:br_eq
    ~graph:as_graph
    ~attrs_v
    out all_branches
  ;
  () (* TODO *)
