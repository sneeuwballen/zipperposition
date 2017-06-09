
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

type info = UntypedAST.attr

type infos = info list

(** Classification of proof steps *)
type kind =
  | Inference of rule * check
  | Simplification of rule * check
  | Esa of rule * check
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
  | C_stmt of Statement.clause_t

(** A proof step, without the conclusion *)
type step = {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: parent list;
  infos: UntypedAST.attr list; (* additional info *)
}

and parent =
  | P_of of proof
  | P_subst of parent * Scoped.scope * Subst.t

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

  (* restrict subst to [sc_p] *)
  let from_subst (p,sc_p) subst: t =
    let subst = Subst.restrict_scope subst sc_p in
    if Subst.is_empty subst
    then P_of p
    else P_subst (P_of p,sc_p,subst)

  let add_subst (p,sc_p) subst: t =
    if Subst.is_empty subst then p
    else P_subst (p,sc_p,subst)

  let rec proof = function
    | P_of p -> p
    | P_subst (p,_,_) -> proof p

  let rec subst = function
    | P_of _ -> []
    | P_subst (p,_,s) -> s :: subst p
end

module Kind = struct
  type t = kind

  let _pp_parent out = function
    | `Name i -> Format.fprintf out "%d" i
    | `Theory s -> Format.fprintf out "theory(%s)" s

  let pp out k = match k with
    | Assert src -> Stmt.Src.pp out src
    | Goal src -> Format.fprintf out "goal %a" Stmt.Src.pp src
    | Lemma -> Format.fprintf out "lemma"
    | Data (src, _) -> Format.fprintf out "data %a" Stmt.Src.pp src
    | Inference (rule,_) ->
      Format.fprintf out "inf %a" Rule.pp rule
    | Simplification (rule,_) ->
      Format.fprintf out "simp %a" Rule.pp rule
    | Esa (rule,_) ->
      Format.fprintf out "esa %a" Rule.pp rule
    | Trivial -> CCFormat.string out "trivial"
    | By_def id -> Format.fprintf out "by_def(%a)" ID.pp id

  let pp_tstp out (k,parents) =
    let pp_parents = Util.pp_list _pp_parent in
    let pp_step status out (rule,parents) = match parents with
      | [] ->
        Format.fprintf out "inference(@[%a,@ [status(%s)]@])" Rule.pp rule status
      | _::_ ->
        Format.fprintf out "inference(@[%a,@ [status(%s)],@ [@[%a@]]@])"
          Rule.pp rule status pp_parents parents
    in
    begin match k with
      | Assert src
      | Goal src -> Stmt.Src.pp_tstp out src
      | Data _ -> Util.error ~where:"ProofStep" "cannot print `Data` step in TPTP"
      | Inference (rule,_)
      | Simplification (rule,_) -> pp_step "thm" out (rule,parents)
      | Esa (rule,_) -> pp_step "esa" out (rule,parents)
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
    | C_stmt _ -> 4

  let compare a b = match a, b with
    | Clause c1, Clause c2 -> SClause.compare c1 c2
    | Form f1, Form f2 -> TypedSTerm.compare f1 f2
    | BoolClause l1, BoolClause l2 -> CCOrd.list BBox.Lit.compare l1 l2
    | Stmt s1, Stmt s2 -> Statement.compare s1 s2
    | C_stmt s1, C_stmt s2 -> Statement.compare s1 s2
    | Clause _, _
    | Form _, _
    | BoolClause _, _
    | Stmt _, _
    | C_stmt _, _
      -> CCInt.compare (res_to_int_ a) (res_to_int_ b)

  let equal a b = match a, b with
    | Clause c1, Clause c2 -> SClause.equal c1 c2
    | Form f1, Form f2 -> TypedSTerm.equal f1 f2
    | BoolClause l1, BoolClause l2 -> CCList.equal BBox.Lit.equal l1 l2
    | Stmt s1, Stmt s2 -> Statement.compare s1 s2 = 0
    | C_stmt s1, C_stmt s2 -> Statement.compare s1 s2 = 0
    | Clause _, _
    | Form _, _
    | BoolClause _, _
    | Stmt _, _
    | C_stmt _, _
      -> false

  let pp out = function
    | Form f -> TypedSTerm.pp out f
    | Clause c ->
      Format.fprintf out "%a/%d" SClause.pp c (SClause.id c)
    | BoolClause lits -> BBox.pp_bclause out lits
    | Stmt stmt -> Statement.pp_input out stmt
    | C_stmt st -> Statement.pp_clause out st

  let c_to_sform ctx c = SClause.to_s_form ~ctx c |> F.close_forall

  let to_s_form ?(ctx=Term.Conv.create()) (r:t): form = match r with
    | Form f -> f
    | Clause c -> SClause.to_s_form ~ctx c |> F.close_forall
    | BoolClause c ->
      List.map BBox.to_s_form c |> F.or_
    | Stmt st ->
      (* assimilate the statement to its formulas *)
      Stmt.Seq.forms st |> Sequence.to_list |> F.and_
    | C_stmt st ->
      (* assimilate the statement to its formulas *)
      Stmt.Seq.forms st
      |> Sequence.map
        (fun c ->
           c
           |> List.map
             (fun lit ->
                SLiteral.map lit ~f:(Term.Conv.to_simple_term ctx)
                |> SLiteral.to_form)
           |> F.or_ |> F.close_forall)
      |> Sequence.to_list |> F.and_
end

module Step = struct
  type t = step

  let equal p1 p2 = p1.id=p2.id
  let compare p1 p2 = CCInt.compare p1.id p2.id
  let hash p = p.id

  let kind p = p.kind
  let parents p = p.parents
  let infos p = p.infos

  let rule p = match p.kind with
    | Trivial
    | Lemma
    | Assert _
    | Data _
    | By_def _
    | Goal _-> None
    | Esa (rule,_)
    | Simplification (rule,_)
    | Inference (rule,_)
      -> Some rule

  let is_assert p = match p.kind with Assert _ -> true | _ -> false
  let is_goal p = match p.kind with Goal _ | Lemma -> true | _ -> false
  let is_trivial p = match p.kind with Trivial -> true | _ -> false
  let is_by_def p = match p.kind with By_def _ -> true | _ -> false

  let distance_to_goal p = p.dist_to_goal

  let get_id_ =
    let n = ref 0 in
    fun () -> CCRef.incr_then_get n

  let trivial = {id=get_id_(); parents=[]; kind=Trivial; dist_to_goal=None; infos=[]; }
  let by_def id = {id=get_id_(); parents=[]; kind=By_def id; dist_to_goal=None; infos=[]; }
  let lemma = {id=get_id_(); parents=[]; kind=Lemma; dist_to_goal=Some 0; infos=[]; }

  let combine_dist o p = match o, (Parent.proof p).step.dist_to_goal with
    | None, None -> None
    | (Some _ as res), None
    | None, (Some _ as res) -> res
    | Some x, Some y -> Some (min x y)

  let step_ ?(infos=[]) kind parents =
    (* distance to goal (0 if a goal itself) *)
    let dist_to_goal = match kind with
      | Goal _ | Lemma -> Some 0
      | _ ->
        let d = match parents with
          | [] -> None
          | [p] -> (Parent.proof p).step.dist_to_goal
          | [p1;p2] -> combine_dist (Parent.proof p1).step.dist_to_goal p2
          | p::l -> List.fold_left combine_dist (Parent.proof p).step.dist_to_goal l
        in
        match kind with
          | Inference _ -> CCOpt.map succ d
          | _ -> d
    in
    { id=get_id_(); kind; parents; dist_to_goal; infos; }

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

  let inference ?infos ?(check=default_check) ~rule parents =
    step_ ?infos (Inference (rule,check)) parents

  let simp ?infos ?(check=default_check) ~rule parents =
    step_ ?infos (Simplification (rule,check)) parents

  let esa ?infos ?(check=default_check) ~rule parents =
    step_ ?infos (Esa (rule,check)) parents

  let pp_infos out = function
    | [] -> ()
    | l ->
      Format.fprintf out "@ %a" (Util.pp_list ~sep:" " UntypedAST.pp_attr) l

  let pp out step = match kind step with
    | Assert _
    | Goal _ ->
      Format.fprintf out "@[<hv2>%a@]%a@," Kind.pp (kind step) pp_infos step.infos
    | Data _ ->
      Format.fprintf out "@[<hv2>data %a@]%a@," Kind.pp (kind step) pp_infos step.infos
    | Lemma -> Format.fprintf out "@[<2>lemma%a@]" pp_infos step.infos
    | Trivial -> Format.fprintf out "@[<2>trivial%a@]" pp_infos step.infos
    | By_def id -> Format.fprintf out "@[<2>by_def %a%a@]" ID.pp id pp_infos step.infos
    | Inference _
    | Simplification _
    | Esa _ ->
      Format.fprintf out "@[<hv2>%a@ with @[<hv>%a@]%a@]@,"
        Kind.pp (kind step)
        (Util.pp_list Result.pp)
        (List.map (fun p -> (Parent.proof p).result) @@ parents step)
        pp_infos step.infos
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
  let mk_c_stmt step stmt = {step; result=C_stmt stmt }

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
      (fun p ->
         let st = step p in
         match Step.rule st with
         | None -> Sequence.empty
         | Some rule ->
           st
           |> Step.parents
           |> Sequence.of_list
           |> Sequence.map
             (fun p' -> (rule,Parent.subst p',Step.infos st), Parent.proof p'))

  (** {2 IO} *)

  let pp_result_of out proof = Result.pp out @@ result proof

  let pp_notrec out p =
    Format.fprintf out "@[%a by %a@]"
      pp_result_of p Kind.pp (Step.kind @@ step p)

  let traverse_bfs ~traversed proof k =
    (* layered BFS *)
    let current, next = ref [proof], ref [] in
    while !current <> [] do
      (* exhaust the current layer of proofs to explore *)
      List.iter (fun proof ->
        if Tbl.mem traversed proof then ()
        else (
          Tbl.add traversed proof ();
          (* traverse premises first *)
          List.iter
            (fun proof' -> next := Parent.proof proof' :: !next)
            (Step.parents @@ step proof);
          (* yield proof *)
          k proof
        ))
        !current;
      (* explore next layer *)
      current := !next;
      next := [];
    done

  let traverse_dfs ~traversed proof k =
    let rec aux proof =
      if Tbl.mem traversed proof then ()
      else (
        Tbl.add traversed proof ();
        (* traverse premises first *)
        List.iter
          (fun p' -> aux (Parent.proof p'))
          (Step.parents @@ step proof);
          (* yield proof *)
        k proof
      )
    in
    aux proof

  let traverse ?(traversed=Tbl.create 16) ~order proof k =
    match order with
      | `BFS -> traverse_bfs ~traversed proof k
      | `DFS -> traverse_dfs ~traversed proof k

  let pp_normal out proof =
    let sep = "by" in
    Format.fprintf out "@[<v>";
    let pp_bullet out = Format.fprintf out "@<1>@{<Green>*@}" in
    traverse ~order:`DFS proof
      (fun p ->
         Format.fprintf out "@[<hv2>%t @[%a@] %s@ %a@]@,"
           pp_bullet Result.pp (result p) sep Step.pp (step p));
    Format.fprintf out "@]"

  let pp_tstp out proof =
    let namespace = Tbl.create 5 in
    Format.fprintf out "@[<v>";
    traverse ~order:`DFS proof
      (fun p ->
         let name = get_name ~namespace p in
         let parents =
           List.map (fun p -> `Name (get_name ~namespace @@ Parent.proof p))
             (Step.parents @@ step p)
         in
         let role = "plain" in (* TODO *)
         let pp_infos out = function
           | [] -> ()
           | l ->
             Format.fprintf out ",@ [@[<hv>%a@]]"
               (Util.pp_list ~sep:", " UntypedAST.pp_attr_tstp) l
         in
         let infos = p.step |> Step.infos in
         begin match result p with
           | Form f ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]%a).@]@,"
               name role TypedSTerm.TPTP.pp f
               Kind.pp_tstp (Step.kind @@ step p,parents) pp_infos infos
           | BoolClause c ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]%a).@]@,"
               name role (Util.pp_list ~sep:" | " BBox.pp_tstp) c
               Kind.pp_tstp (Step.kind @@ step p,parents) pp_infos infos
           | Clause c ->
             Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]%a).@]@,"
               name role SClause.pp_tstp c
               Kind.pp_tstp (Step.kind @@ step p,parents) pp_infos infos
           | Stmt stmt ->
             let module T = TypedSTerm in
             Statement.TPTP.pp T.TPTP.pp T.TPTP.pp T.TPTP.pp out stmt
           | C_stmt stmt ->
             let pp_t = Term.TPTP.pp in
             let pp_c = Util.pp_list ~sep:"|" (SLiteral.TPTP.pp pp_t) in
             Statement.TPTP.pp pp_c pp_t Type.TPTP.pp out stmt
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
        let label = _to_str_escape "@[<2>%a@]@." pp_result_of p in
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
      ~attrs_e:(fun (r,s,infos) ->
        let pp_subst out s =
          Format.fprintf out "@,{@[<v>%a@]}" Subst.pp_bindings s
        in
        let label =
          if s=[] && infos=[] then Rule.name r
          else _to_str_escape "@[<v>%s%a%a@]"
              (Rule.name r) (Util.pp_list ~sep:"" pp_subst) s Step.pp_infos infos
        in
        [`Label label; `Other ("dir", "back")])
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
    and conv_step ?(ctx=Term.Conv.create()) p =
      let res = Result.to_s_form ~ctx (result p) in
      let parents =
        List.map (conv_parent ~ctx) (Step.parents @@ step p)
      in
      begin match Step.kind @@ step p with
        | Inference (name,c)
        | Simplification (name,c) -> LLProof.inference c res name parents
        | Esa (name,c) -> LLProof.esa c res name parents
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
        let p' = conv_parent ~ctx p in
        (* build instance of result *)
        let res_subst = match result @@ Parent.proof p with
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
                   |> Term.Conv.var_to_simple_var ctx
                 and t' =
                   Term.of_term_unsafe t
                   |> Term.Conv.to_simple_term ctx in
                 Some (v',t')
               ) else None)
          |> Var.Subst.of_seq
        in
        LLProof.instantiate res_subst subst p'
    in
    conv p

  module Src_tbl = CCHashtbl.Make(struct
      type t = Stmt.source
      let equal = Stmt.Src.equal
      let hash = Stmt.Src.hash
    end)

  (* used to share the same SClause.t in the proof *)
  let input_proof_tbl_ : Step.t Src_tbl.t = Src_tbl.create 32

  let rule_neg_ = Rule.mk "neg_goal"
  let rule_cnf_ = Rule.mk "cnf"
  let rule_renaming_ = Rule.mk "renaming"
  let rule_define_ = Rule.mk "define"
  let rule_preprocess_ msg = Rule.mkf "preprocess(%s)" msg

  let rec step_of_src src: step =
    try Src_tbl.find input_proof_tbl_ src
    with Not_found ->
      let p = match Stmt.Src.view src with
        | Stmt.Input (_, Stmt.R_goal) -> Step.goal src
        | Stmt.Input (_, _) -> Step.assert_ src
        | Stmt.From_file (_, Stmt.R_goal) -> Step.goal src
        | Stmt.From_file (_, _) -> Step.assert_ src
        | Stmt.Internal _ -> Step.trivial
        | Stmt.Neg srcd -> Step.esa ~rule:rule_neg_ [parent_of_sourced srcd]
        | Stmt.CNF srcd -> Step.esa ~rule:rule_cnf_ [parent_of_sourced srcd]
        | Stmt.Preprocess (srcd,l,msg) ->
          Step.esa ~rule:(rule_preprocess_ msg)
            (parent_of_sourced srcd :: List.map parent_of_sourced l)
        | Stmt.Renaming (srcd,id,form) ->
          Step.esa ~rule:rule_renaming_
            [parent_of_sourced srcd;
             Parent.from @@ mk_f_by_def id @@
               TypedSTerm.(Form.eq (const id ~ty:Ty.prop) form)]
        | Stmt.Define id -> Step.by_def id
      in
      Src_tbl.add input_proof_tbl_ src p;
      p

  and parent_of_sourced (res, src) : Parent.t =
    let p = step_of_src src in
    begin match res with
      | Stmt.Sourced_input f ->
        Parent.from (mk_f p f)
      | Stmt.Sourced_clause c ->
        let lits = List.map Literal.Conv.of_form c |> Array.of_list in
        let c = SClause.make ~trail:Trail.empty lits in
        Parent.from (mk_c p c)
      | Stmt.Sourced_statement stmt ->
        Parent.from (mk_stmt p stmt)
      | Stmt.Sourced_clause_stmt stmt ->
        Parent.from (mk_c_stmt p stmt)
    end
end
