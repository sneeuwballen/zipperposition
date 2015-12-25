
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Common stuff for Induction} *)

open Libzipperposition

module T = FOTerm
module Ty = Type
module Lits = Literals

type term = FOTerm.t
type formula = TypedSTerm.t

let section = Util.Section.make ~parent:Const.section "ind"

let ind_types_ = ref []
let cover_set_depth_ = ref 1
let show_lemmas_ = ref false

let ind_types () = !ind_types_
let cover_set_depth () = !cover_set_depth_

(* is [s] a constructor symbol for some inductive type? *)
let is_constructor s =
  List.exists
    (fun (_, cstors) -> CCList.Set.mem ~eq:ID.equal s cstors)
    !ind_types_

let on_enable = Signal.create()

type kind = [`Full | `Simple]

let enabled_ = ref false
let enable_ () =
  if not !enabled_ then (
    enabled_ := true;
    Util.debugf ~section 1
      "@[Induction: requires ord=rpo6; select=NoSelection@]" (fun k->k);
    Params.ord := "rpo6";   (* new default! RPO is necessary*)
    Params.select := "NoSelection";
    Signal.send on_enable ();
  )

let declare_ ty cstors =
  (* remember to declare this type as inductive *)
  Util.debugf ~section 1 "@[user declares inductive type %a = %a@]"
    (fun k->k ID.pp ty (Util.pp_list ID.pp) cstors);
  ind_types_ := (ty, cstors) :: !ind_types_;
  enable_();
  ()

let constr_cstors =
  let module C = Libzipperposition.Comparison in
  let cmp s1 s2 = match is_constructor s1, is_constructor s2 with
    | true, true
    | false, false -> 0
    | true, false -> -1
    | false, true -> 1
  in
  Precedence.Constr.make cmp

module Make(Ctx : Ctx.S) = struct
  (* declare a list of inductive types *)
  let declare_types () =
    List.iter
      (fun (ty,cstors) ->
         (* TODO: support polymorphic types? *)
         let pattern = Ty.const ty in
         let constructors = List.map
             (fun s ->
                match Ctx.find_signature s with
                | None ->
                    let msg = CCFormat.sprintf
                        "cannot find the type of inductive constructor %a" ID.pp s
                    in failwith msg
                | Some ty ->
                    s, ty
             ) cstors
         in
         (* declare type. *)
         ignore (Ctx.Induction.declare_ty pattern constructors);
         Util.debugf ~section 1 "declare inductive type %a" (fun k->k Ty.pp pattern);
         ()
      ) !ind_types_

  (* true if [t = c] where [c] is some inductive constructor
      such as "cons" or "node" *)
  let is_a_constructor t = match T.Classic.view t with
    | T.Classic.App (s, _) ->
        Sequence.mem ~eq:ID.equal s Ctx.Induction.Seq.constructors
    | _ -> false

  (* find inductive constants in clauses of [seq] *)
  let find_inductive_cst lits : T.t Sequence.t =
    Lits.Seq.terms lits
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t ->
         T.is_ground t
         && T.is_const t
         && not (Ctx.Induction.is_blocked t)
         && Ctx.Induction.is_inductive_type (T.ty t)
         && not (is_a_constructor t)   (* 0 and nil: not inductive const *)
      )

  (* ensure s1 > s2 if s1 is an inductive constant
      and s2 is a sub-case of s1 *)
  let constr_sub_cst s1 s2 =
    let module C = Libzipperposition.Comparison in
    let res =
      if Ctx.Induction.is_inductive_symbol s1 && Ctx.Induction.dominates s1 s2
      then C.Gt
      else if Ctx.Induction.is_inductive_symbol s2 && Ctx.Induction.dominates s2 s1
      then C.Lt
      else C.Incomparable
    in res

  let () =
    Ctx.set_selection_fun Selection.no_select
end

module MakeAvatar(A : Avatar.S) = struct
  module Env = A.E
  module Ctx = Env.Ctx
  module BoolLit = Ctx.BoolLit
  module C = Env.C
  module CI = Ctx.Induction

  (* terms that are either inductive constants or sub-constants *)
  let constants_or_sub c =
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t -> CI.is_inductive t || CI.is_sub_constant t)
    |> Sequence.sort_uniq ~cmp:T.compare
    |> Sequence.to_rev_list

  (* sub-terms of an inductive type, that occur several times (candidate
     for "subterm generalization" *)
  let generalizable_subterms c =
    let count = T.Tbl.create 16 in
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t -> CI.is_inductive_type (T.ty t) && not (T.is_const t))
    |> Sequence.iter
      (fun t ->
         let n = try T.Tbl.find count t with Not_found -> 0 in
         T.Tbl.replace count t (n+1)
      );
    (* terms that occur more than once *)
    T.Tbl.to_seq count
    |> Sequence.filter_map (fun (t,n) -> if n>1 then Some t else None)
    |> Sequence.to_rev_list

  (* apply the list of replacements [l] to the term [t] *)
  let replace_many l t =
    List.fold_left
      (fun t (old,by) -> T.replace t ~old ~by)
      t l

  let flag_cut_introduced = C.new_flag()

  let is_ind_conjecture_ c =
    match C.distance_to_conjecture c with
    | Some (0 | 1) -> true
    | Some _
    | None -> false

  let has_pos_lit_ c =
    CCArray.exists Literal.is_pos (C.lits c)

  let is_acceptable_lemma lits =
    (* not too deep *)
    Lits.Seq.terms lits
    |> Sequence.map T.depth
    |> Sequence.max
    |> CCOpt.maybe (fun d -> d < 5) true

  let lemmas_ = ref []

  (* when a unit clause has inductive constants, take its negation
      and add it as a lemma (some restrictions apply) *)
  let inf_introduce_lemmas c =
    let ind_csts = constants_or_sub c in
    let generalize ~on lit =
      (* fresh var generator *)
      let mk_fresh_var_ =
        let r = ref 0 in
        fun ty ->
          let v = T.var_of_int ~ty !r in
          incr r;
          v
      in
      (* abstract w.r.t all those constants (including the term
          being generalized). The latter must occur first, as it
          might contain constants being replaced. *)
      let replacements =
        List.map (fun t -> t, mk_fresh_var_ (T.ty t)) (on @ ind_csts) in
      (* replace constants by variables in [lit], then
          let [c] be [forall... (not lit)] *)
      let lit =
        lit
        |> Literal.map (replace_many replacements)
        |> Literal.negate
      in
      let lits = [| lit |] in
      (* if [box lits] already exists or is too deep, no need to re-do inference *)
      if not (is_acceptable_lemma lits)
      then []
      else (
        (* introduce cut now *)
        let proof cc = Proof.mk_c_trivial ~theories:["ind"] ~info:["cut"] cc in
        let clauses, _ = A.introduce_cut lits proof in
        List.iter (fun c -> C.set_flag flag_cut_introduced c true) clauses;
        Util.debugf ~section 2
          "@[<2>introduce cut@ from %a@ @[<hv0>%a@]@ generalizing on @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses
              (Util.pp_list T.pp) on);
        lemmas_ := List.rev_append clauses !lemmas_;
        clauses
      )
    in
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag flag_cut_introduced c)
    && C.is_unit_clause c
    && not (has_pos_lit_ c) (* only positive lemmas, therefore C negative *)
    && not (CCList.is_empty ind_csts) (* && not (T.Set.for_all CI.is_inductive set) *)
    then (
      assert (Array.length (C.lits c) = 1);
      let lit = (C.lits c).(0) in
      let terms = generalizable_subterms c in
      (* first, lemma without generalization;
          then, each possible way to generalize a subterm occurring multiple times *)
      List.rev_append
        (generalize ~on:[] lit)
        (CCList.flat_map (fun t -> generalize ~on:[t] lit) terms)
    ) else []

  let show_lemmas () =
    Util.debugf ~section 1 "@[<2>lemmas:@ [@[<hv>%a@]]@]"
      (fun k->k (Util.pp_list C.pp) !lemmas_)

  let () =
    Signal.on Signals.on_exit
      (fun _ ->
         if !show_lemmas_ then show_lemmas ();
         Signal.ContinueListening
      );
end

module A = Libzipperposition_parsers.Ast_tptp

let init_from_decls pairs =
  let get_str = function
    | A.GNode (s, []) | A.GString s -> s
    | _ -> raise Exit
  in
  (* search for "inductive(c1, c2, ...)" *)
  let rec scan_for_constructors = function
    | A.GNode ("inductive", l) :: tail when List.length l >= 2 ->
        begin try
            let constructors = List.map get_str l in
            Some constructors
          with Exit ->
            scan_for_constructors tail
        end
    | _ :: tail -> scan_for_constructors tail
    | []  -> None
  in
  Sequence.iter
    (fun (ty, info) -> match scan_for_constructors info with
       | None -> ()
       | Some l ->
         assert false (* FIXME: should be done on Cnf.TyDecl? declare_ ty l *)
     )
    pairs

let () =
  Params.add_opts
    [ "--induction", Arg.Unit (Signal.send on_enable), " enable induction"
    ; "--induction-depth", Arg.Set_int cover_set_depth_,
        " set default induction depth"
    ; "--show-lemmas", Arg.Set show_lemmas_, " show inductive (candidate) lemmas"
    ]
