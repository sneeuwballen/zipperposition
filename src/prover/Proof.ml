
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Logtk

module Loc = ParseLocation
module Stmt = Statement
module T = TypedSTerm
module F = T.Form

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make ~parent:Const.section "proof"

type statement_src = Statement.source

type rule = string

type check = [`No_check | `Check | `Check_with of form list]

(** Classification of proof steps *)
type kind =
  | Inference of rule * string option * check
  | Simplification of rule * string option * check
  | Esa of rule * string option * check
  | Assert of statement_src
  | Goal of statement_src
  | Lemma
  | Data of statement_src * Type.t Statement.data
  | Trivial (** trivial, or trivial within theories *)
  | By_def of ID.t

type result =
  | Form of form
  | Clause of SClause.t
  | BoolClause of bool_lit list
  | Stmt of Statement.input_t

(** A proof step, without the conclusion *)
type step = {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: parent list;
}

and parent =
  | P_of of proof
  | P_subst of proof * Scoped.scope * Subst.t

(** Proof Step with its conclusion *)
and proof = {
  step: step;
  result : result;
}

type t = proof

module Rule = struct
  type t = rule
  let pp out r = Format.fprintf out "'%s'" r
  let name r = r
  let mk name = name
  let mkf fmt = CCFormat.ksprintf ~f:mk fmt
end

module Parent = struct
  type t = parent

  let from p: t = P_of p
  let from_subst (p,sc_p) subst: t = P_subst (p,sc_p,subst)

  let proof = function
    | P_of p -> p
    | P_subst (p,_,_) -> p
end

module Kind = struct
  type t = kind

  let _pp_parent out = function
    | `Name i -> Format.fprintf out "%d" i
    | `Theory s -> Format.fprintf out "theory(%s)" s

  let pp_comment out = function
    | None -> ()
    | Some s -> Format.fprintf out " %s" s

  let pp out k = match k with
    | Assert src -> Stmt.Src.pp out src
    | Goal src -> Format.fprintf out "goal %a" Stmt.Src.pp src
    | Lemma -> Format.fprintf out "lemma"
    | Data (src, _) -> Format.fprintf out "data %a" Stmt.Src.pp src
    | Inference (rule,c,_) ->
      Format.fprintf out "inf %a%a" Rule.pp rule pp_comment c
    | Simplification (rule,c,_) ->
      Format.fprintf out "simp %a%a" Rule.pp rule pp_comment c
    | Esa (rule,c,_) ->
      Format.fprintf out "esa %a%a" Rule.pp rule pp_comment c
    | Trivial -> CCFormat.string out "trivial"
    | By_def id -> Format.fprintf out "by_def(%a)" ID.pp id

  let pp_tstp out (k,parents) =
    let pp_parents = Util.pp_list _pp_parent in
    let pp_step status out (rule,parents) = match parents with
      | [] ->
        Format.fprintf out "inference(%a, [status(%s)])" Rule.pp rule status
      | _::_ ->
        Format.fprintf out "inference(%a, [status(%s)], [%a])"
          Rule.pp rule status pp_parents parents
    in
    begin match k with
      | Assert src
      | Goal src -> Stmt.Src.pp_tstp out src
      | Data _ -> Util.error ~where:"ProofStep" "cannot print `Data` step in TPTP"
      | Inference (rule,_,_)
      | Simplification (rule,_,_) -> pp_step "thm" out (rule,parents)
      | Esa (rule,_,_) -> pp_step "esa" out (rule,parents)
      | Lemma -> Format.fprintf out "lemma"
      | Trivial -> assert(parents=[]); Format.fprintf out "trivial([status(thm)])"
      | By_def _ -> Format.fprintf out "by_def([status(thm)])"
    end
end

module Result = struct
  type t = result

  let res_to_int_ = function
    | Clause _ -> 0
    | Form _ -> 1
    | BoolClause _ -> 2
    | Stmt _ -> 3

  let compare a b = match a, b with
    | Clause c1, Clause c2 -> SClause.compare c1 c2
    | Form f1, Form f2 -> TypedSTerm.compare f1 f2
    | BoolClause l1, BoolClause l2 -> CCOrd.list BBox.Lit.compare l1 l2
    | Stmt s1, Stmt s2 -> Statement.compare s1 s2
    | Clause _, _
    | Form _, _
    | BoolClause _, _
    | Stmt _, _
      -> CCInt.compare (res_to_int_ a) (res_to_int_ b)

  let equal a b = match a, b with
    | Clause c1, Clause c2 -> SClause.equal c1 c2
    | Form f1, Form f2 -> TypedSTerm.equal f1 f2
    | BoolClause l1, BoolClause l2 -> CCList.equal BBox.Lit.equal l1 l2
    | Stmt s1, Stmt s2 -> Statement.compare s1 s2 = 0
    | Clause _, _
    | Form _, _
    | BoolClause _, _
    | Stmt _, _
      -> false

  let pp out = function
    | Form f -> TypedSTerm.pp out f
    | Clause c ->
      Format.fprintf out "%a/%d" SClause.pp c (SClause.id c)
    | BoolClause lits -> BBox.pp_bclause out lits
    | Stmt stmt -> Statement.pp_input out stmt

  let to_s_form ?(ctx=FOTerm.Conv.create()) (r:t): form = match r with
    | Form f -> f
    | Clause c -> SClause.to_s_form ~ctx c |> F.close_forall
    | BoolClause c ->
      List.map BBox.to_s_form c |> F.or_
    | Stmt st ->
      (* assimilate the statement to its formulas *)
      Stmt.Seq.forms st |> Sequence.to_list |> F.and_
end

module Step = struct
  type t = step

  let equal p1 p2 = p1.id=p2.id
  let compare p1 p2 = CCInt.compare p1.id p2.id
  let hash p = p.id

  let kind p = p.kind
  let parents p = p.parents

  let rule p = match p.kind with
    | Trivial
    | Lemma
    | Assert _
    | Data _
    | By_def _
    | Goal _-> None
    | Esa (rule,_,_)
    | Simplification (rule,_,_)
    | Inference (rule,_,_)
      -> Some rule

  let comment p = match p.kind with
    | Trivial
    | Lemma
    | Assert _
    | By_def _
    | Data _
    | Goal _-> None
    | Esa (_,c,_)
    | Simplification (_,c,_)
    | Inference (_,c,_)
      -> Some c

  let is_assert p = match p.kind with Assert _ -> true | _ -> false
  let is_goal p = match p.kind with Goal _ | Lemma -> true | _ -> false
  let is_trivial p = match p.kind with Trivial -> true | _ -> false
  let is_by_def p = match p.kind with By_def _ -> true | _ -> false

  let distance_to_goal p = p.dist_to_goal

  let get_id_ =
    let n = ref 0 in
    fun () -> CCRef.incr_then_get n

  let trivial = {id=get_id_(); parents=[]; kind=Trivial; dist_to_goal=None; }
  let by_def id = {id=get_id_(); parents=[]; kind=By_def id; dist_to_goal=None }
  let lemma = {id=get_id_(); parents=[]; kind=Lemma; dist_to_goal=Some 0; }

  let combine_dist o p = match o, (Parent.proof p).step.dist_to_goal with
    | None, None -> None
    | (Some _ as res), None
    | None, (Some _ as res) -> res
    | Some x, Some y -> Some (max x y)

  let step_ kind parents =
    (* distance to goal *)
    let dist_to_goal = match parents with
      | [] -> None
      | [p] -> CCOpt.map succ (Parent.proof p).step.dist_to_goal
      | [p1;p2] ->
        CCOpt.map succ (combine_dist (Parent.proof p1).step.dist_to_goal p2)
      | p::l ->
        CCOpt.map succ (List.fold_left combine_dist (Parent.proof p).step.dist_to_goal l)
    in
    { id=get_id_(); kind; parents; dist_to_goal; }

  let data src data =
    step_ (Data (src,data)) []

  let assert_ src = step_ (Assert src) []

  let goal src = step_ (Goal src) []

  let assert' ?loc ~file ~name () =
    let src = Stmt.Src.from_file ?loc ~name file Stmt.R_assert in
    assert_ src

  let goal' ?loc ~file ~name () =
    let src = Stmt.Src.from_file ?loc ~name file Stmt.R_goal in
    goal src

  let default_check : check = `Check

  let inference ?(check=default_check) ?comment ~rule parents =
    step_ (Inference (rule,comment,check)) parents

  let simp ?(check=default_check) ?comment ~rule parents =
    step_ (Simplification (rule,comment,check)) parents

  let esa ?(check=default_check) ?comment ~rule parents =
    step_ (Esa (rule,comment,check)) parents

  let pp out step = match kind step with
    | Assert _
    | Goal _ ->
      Format.fprintf out "@[<hv2>%a@]@," Kind.pp (kind step)
    | Data _ ->
      Format.fprintf out "@[<hv2>data %a@]@," Kind.pp (kind step)
    | Lemma -> Format.fprintf out "lemma"
    | Trivial -> Format.fprintf out "trivial"
    | By_def id -> Format.fprintf out "by_def %a" ID.pp id
    | Inference _
    | Simplification _
    | Esa _ ->
      Format.fprintf out "@[<hv2>%a@ with @[<hv>%a@]@]@,"
        Kind.pp (kind step)
        (Util.pp_list Result.pp)
        (List.map (fun p -> (Parent.proof p).result) @@ parents step)
end

module S = struct
  type t = proof

  let result p = p.result
  let step p = p.step

  let compare a b =
    let (<?>) = CCOrd.(<?>) in
    compare a.step b.step <?> (Result.compare, a.result, b.result)

  let equal a b =
    Step.equal a.step b.step && Result.equal a.result b.result

  let hash a = Step.hash a.step

  let compare_by_result a b = Result.compare a.result b.result

  module Tbl = CCHashtbl.Make(struct
      type t = proof
      let equal = equal
      let hash = hash
    end)

  let has_absurd_lits p = match result p with
    | Clause c -> Literals.is_absurd (SClause.lits c)
    | _ -> false

  let is_proof_of_false p = match result p with
    | Form f when TypedSTerm.equal f TypedSTerm.Form.false_ -> true
    | Clause c ->
      Literals.is_absurd (SClause.lits c) && Trail.is_empty (SClause.trail c)
    | _ -> false

  let is_pure_bool p = match result p with
    | BoolClause _ -> true
    | _ -> false

  let mk_f step res = {step; result=Form res; }

  let mk_f_trivial = mk_f Step.trivial
  let mk_f_by_def id f = mk_f (Step.by_def id) f

  let mk_f_inference ?check ~rule f parents =
    let step = Step.inference ?check ~rule parents in
    mk_f step f

  let mk_f_simp ?check ~rule f parents =
    let step = Step.simp ?check ~rule parents in
    mk_f step f

  let mk_f_esa ?check ~rule f parents =
    let step = Step.esa ?check ~rule parents in
    mk_f step f

  let mk_c step c = {step; result=Clause c; }
  let mk_bc step c = {step; result=BoolClause c; }
  let mk_stmt step stmt = {step; result=Stmt stmt; }

  let adapt_c p c =
    { p with result=Clause c; }

  let adapt_f p f =
    { p with result=Form f; }

  let get_name ~namespace p =
    try
      Tbl.find namespace p
    with Not_found ->
      let n = Tbl.length namespace in
      Tbl.add namespace p n;
      n

  (** {2 Conversion to a graph of proofs} *)

  (** Get a graph of the proof *)
  let as_graph =
    CCGraph.make
      (fun p -> match Step.rule (step p) with
         | None -> Sequence.empty
         | Some rule ->
           let parents = Sequence.of_list (Step.parents @@ step p) in
           Sequence.map (fun p' -> rule, Parent.proof p') parents)

  (** {2 IO} *)

  let pp_result_of out proof = Result.pp out @@ result proof

  let pp_notrec out p =
    Format.fprintf out "@[%a by %a@]"
      pp_result_of p Kind.pp (Step.kind @@ step p)

  let traverse ?(traversed=Tbl.create 16) proof k =
    let current, next = ref [proof], ref [] in
    while !current <> [] do
      (* exhaust the current layer of proofs to explore *)
      while !current <> [] do
        let proof = List.hd !current in
        current := List.tl !current;
        if Tbl.mem traversed proof then ()
        else (
          Tbl.add traversed proof ();
          (* traverse premises first *)
          List.iter
            (fun proof' -> next := Parent.proof proof' :: !next)
            (Step.parents @@ step proof);
          (* yield proof *)
          k proof;
        )
      done;
      (* explore next layer *)
      current := !next;
      next := [];
    done

  let pp_normal out proof =
    let sep = "by" in
    Format.fprintf out "@[<v>";
    let pp_bullet out = Format.fprintf out "@<1>@{<Green>*@}" in
    traverse proof
      (fun p ->
         Format.fprintf out "@[<hv2>%t @[%a@] %s@ %a@]@,"
           pp_bullet Result.pp (result p) sep Step.pp (step p)
      );
    Format.fprintf out "@]"

  let pp_tstp out proof =
    let namespace = Tbl.create 5 in
    Format.fprintf out "@[<v>";
    traverse proof
      (fun p ->
         let name = get_name ~namespace p in
         let parents =
           List.map (fun p -> `Name (get_name ~namespace @@ Parent.proof p))
             (Step.parents @@ step p)
         in
         let role = "plain" in (* TODO *)
         begin match result p with
           | Form f ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
               name role TypedSTerm.TPTP.pp f
               Kind.pp_tstp (Step.kind @@ step p,parents)
           | BoolClause c ->
             let tr = Trail.of_list c in
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
               name role SClause.pp_trail_tstp tr
               Kind.pp_tstp (Step.kind @@ step p,parents)
           | Clause c ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]).@]@,"
               name role SClause.pp_tstp c
               Kind.pp_tstp (Step.kind @@ step p,parents)
           | Stmt stmt ->
             let module T = TypedSTerm in
             Statement.pp T.TPTP.pp T.TPTP.pp T.TPTP.pp out stmt
         end);
    Format.fprintf out "@]";
    ()

  (** Prints the proof according to the given input switch *)
  let pp o out proof = match o with
    | Options.Print_none -> Util.debug ~section 1 "proof printing disabled"
    | Options.Print_tptp -> pp_tstp out proof
    | Options.Print_normal -> pp_normal out proof
    | Options.Print_zf -> failwith "proof printing in ZF not implemented" (* TODO? *)

  let _pp_list_str = Util.pp_list CCFormat.string

  let _escape_dot s =
    let b = Buffer.create (String.length s + 5) in
    String.iter
      (fun c ->
         begin match c with
           | '|' | '\\' | '{' | '}' | '<' | '>' | '"' ->
             Buffer.add_char b '\\'; Buffer.add_char b c
           | '\n' -> Buffer.add_string b "\\l"; (* left justify *)
           | _ -> Buffer.add_char b c
         end)
      s;
    Buffer.contents b

  let _to_str_escape fmt =
    Util.ksprintf_noc ~f:_escape_dot fmt

  let pp_dot_seq ~name out seq =
    (* TODO: check proof is a DAG *)
    CCGraph.Dot.pp_seq
      ~tbl:(CCGraph.mk_table ~eq:equal ~hash:hash 64)
      ~eq:equal
      ~name
      ~graph:as_graph
      ~attrs_v:(fun p ->
        let label = _to_str_escape "@[<2>%a@]" pp_result_of p in
        let attrs = [`Label label; `Style "filled"] in
        let shape = `Shape "box" in
        if is_proof_of_false p then [`Color "red"; `Label "[]"; `Shape "box"; `Style "filled"]
        else if is_pure_bool p then `Color "cyan3" :: shape :: attrs
        else if has_absurd_lits p then `Color "orange" :: shape :: attrs
        else if Step.is_assert @@ step p then `Color "yellow" :: shape :: attrs
        else if Step.is_goal @@ step p then `Color "green" :: shape :: attrs
        else if Step.is_trivial @@ step p then `Color "cyan" :: shape :: attrs
        else if Step.is_by_def @@ step p then `Color "navajowhite" :: shape :: attrs
        else shape :: attrs
      )
      ~attrs_e:(fun r ->
        [`Label (Rule.name r); `Other ("dir", "back")])
      out
      seq;
    Format.pp_print_newline out ();
    ()

  let pp_dot ~name out proof = pp_dot_seq ~name out (Sequence.singleton proof)

  let pp_dot_seq_file ?(name="proof") filename seq =
    (* print graph on file *)
    Util.debugf ~section 1 "print proof graph to@ `%s`" (fun k->k filename);
    CCIO.with_out filename
      (fun oc ->
         let out = Format.formatter_of_out_channel oc in
         Format.fprintf out "%a@." (pp_dot_seq ~name) seq)

  let pp_dot_file ?name filename proof =
    pp_dot_seq_file ?name filename (Sequence.singleton proof)

  let to_llproof (p:t): LLProof.t =
    let tbl = Tbl.create 32 in
    let rec conv p: LLProof.t =
      begin match Tbl.get tbl p with
        | Some r -> r
        | None ->
          let res = conv_step p in
          Tbl.add tbl p res;
          res
      end
    and conv_step ?(ctx=FOTerm.Conv.create()) p =
      let res = Result.to_s_form ~ctx (result p) in
      let parents =
        List.map (conv_parent ~ctx) (Step.parents @@ step p)
      in
      begin match Step.kind @@ step p with
        | Inference (name,_,c)
        | Simplification (name,_,c) -> LLProof.inference c res name parents
        | Esa (name,_,c) -> LLProof.esa c res name parents
        | Trivial -> LLProof.trivial res
        | By_def id -> LLProof.by_def id res
        | Assert _ -> LLProof.assert_ res
        | Goal _ -> LLProof.assert_ res
        | Lemma -> LLProof.trivial res
        | Data (_,_) -> assert false (* TODO *)
      end
    and conv_parent ~ctx (p:Parent.t): LLProof.t = match p with
      | P_of p -> conv p
      | P_subst (p,sc_p,subst) ->
        let p' = conv p in
        (* build instance of result *)
        let res_subst = match result p with
          | Clause c ->
            let trail = SClause.trail c in
            let lits' =
              Literals.apply_subst ~renaming:(Subst.Renaming.create())
                subst (SClause.lits c,sc_p)
            in
            let c' = SClause.make ~trail lits' in
            Result.to_s_form ~ctx (Clause c')
          | _ -> assert false
        in
        (* "translate" substitution *)
        let subst =
          Subst.to_seq subst
          |> Sequence.filter_map
            (fun ((v,sc),(t,_)) ->
               if sc=sc_p then (
                 let v' =
                   HVar.cast v ~ty:(Type.of_term_unsafe (HVar.ty v))
                   |> FOTerm.Conv.var_to_simple_var ctx
                 and t' =
                   FOTerm.of_term_unsafe t
                   |> FOTerm.Conv.to_simple_term ctx in
                 Some (v',t')
               ) else None)
          |> Var.Subst.of_seq
        in
        LLProof.instantiate res_subst subst p'
    in
    conv p
end
