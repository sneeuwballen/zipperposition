
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

module Loc = ParseLocation
module T = TypedSTerm
module F = T.Form
module UA = UntypedAST
module Fmt = CCFormat

type term = TypedSTerm.t
type form = TypedSTerm.t
type inst_subst = (term, term) Var.Subst.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make "proof"

type rule = string

type tag = Builtin.Tag.t

type attrs = UntypedAST.attrs

type info = UntypedAST.attr

type infos = info list

type kind =
  | Intro of source * role
  | Inference of rule * tag list
  | Simplification of rule * tag list
  | Esa of rule
  | Trivial (** trivial, or trivial within theories *)
  | Define of ID.t * source (** definition *)
  | By_def of ID.t (** following from the def of ID *)

and source = {
  src_id: int;
  src_view: source_view;
}
and source_view =
  | From_file of from_file * UntypedAST.attrs
  | Internal of attrs

and role =
  | R_assert
  | R_goal
  | R_def
  | R_decl
  | R_lemma

(* a statement in a file *)
and from_file = {
  file : string;
  name : string option;
  loc: ParseLocation.t option;
}

type flavor =
  [ `Pure_bool
  | `Absurd_lits
  | `Proof_of_false
  | `Vanilla
  | `Def
  ]

(** Typeclass for the result of a proof step *)
type 'a result_tc = {
  res_id: int; (* unique ID of the class *)
  res_of_exn: exn -> 'a option;
  res_to_exn: 'a -> exn;
  res_compare: 'a -> 'a -> int;
  res_is_stmt: bool;
  res_pp_in: Output_format.t -> 'a CCFormat.printer;
  res_to_form: ctx:Term.Conv.ctx -> 'a -> TypedSTerm.Form.t;
  res_to_form_subst: ctx:Term.Conv.ctx -> Subst.Projection.t -> 'a -> form * inst_subst;
  res_name:('a -> string) option;
  res_flavor: 'a -> flavor;
}

(** existential type for result of an inference *)
type result = Res : 'a result_tc * exn -> result

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
  | P_subst of proof * Subst.Projection.t

(** Proof Step with its conclusion *)
and proof = {
  step: step;
  result : result;
}

type t = proof

module Tag = Builtin.Tag

module Rule = struct
  type t = rule
  let pp out r = Format.fprintf out "'%s'" r
  let name r = r
  let mk name = name
  let mkf fmt = CCFormat.ksprintf ~f:mk fmt
end

module Src = struct
  type t = source

  let file x = x.file
  let name x = x.name
  let loc x = x.loc

  let equal a b = a.src_id = b.src_id
  let hash a = a.src_id
  let view a = a.src_view

  let mk_ =
    let n = ref 0 in
    fun src_view -> {src_view; src_id=CCRef.get_then_incr n}

  let mk_name_ =
    let n = ref 0 in
    fun () -> Printf.sprintf "zf_stmt_%d" (CCRef.get_then_incr n)

  let from_file ?loc ?name ?(attrs=[]) file : t =
    (* NOTE: we always give a unique name if not present *)
    let name = match name with Some _ -> name | None -> Some(mk_name_()) in
    mk_ (From_file ({ name; loc; file; }, attrs))

  let internal attrs = mk_ (Internal attrs)

  let pp_from_file out x =
    let pp_name out = function
      | None -> ()
      | Some n -> Format.fprintf out "at %s " n
    in
    Format.fprintf out "@[<2>%ain@ `%s`@,%a@]"
      pp_name x.name x.file ParseLocation.pp_opt x.loc

  let pp_role out = function
    | R_decl -> CCFormat.string out "decl"
    | R_assert -> CCFormat.string out "assert"
    | R_goal -> CCFormat.string out "goal"
    | R_def -> CCFormat.string out "def"
    | R_lemma -> CCFormat.string out "lemma"

  let pp_tstp out src = match view src with
    | Internal _ -> ()
    | From_file (src,_) ->
      let file = src.file in
      begin match src.name with
        | None -> Format.fprintf out "file('%s')" file
        | Some name -> Format.fprintf out "file(@['%s',@ '%s'@])" file name
      end

  let pp out src = match view src with
    | Internal _ -> ()
    | From_file (src,attrs) ->
      let file = src.file in
      begin match src.name with
        | None -> Format.fprintf out "'%s'%a" file UA.pp_attrs attrs
        | Some name -> Format.fprintf out "'%s' in '%s'%a" name file UA.pp_attrs attrs
      end

  let to_attrs src : UntypedAST.attrs =
    let open UntypedAST.A in
    begin match view src with
      | Internal attrs -> str "internal" :: attrs
      | From_file (f,attrs) ->
        begin match f.name with
          | None -> app "file" [quoted f.file] :: attrs
          | Some n -> app "file" [quoted f.file; quoted n] :: attrs
        end
    end
end

module Parent = struct
  type t = parent

  let from p: t = P_of p

  let from_subst_proj p (subst:Subst.Projection.t) : t =
    if Subst.Projection.is_empty subst
    then P_of p
    else P_subst (p,subst)

  let from_subst renaming (p,sc_p) subst: t =
    let subst = Subst.Projection.make renaming (subst,sc_p) in
    from_subst_proj p subst

  let proof = function
    | P_of p -> p
    | P_subst (p,_) -> p

  let subst = function
    | P_of _ -> None
    | P_subst (_,s) -> Some s
end

let pp_tag = Tag.pp
let pp_tags out = function
  | [] -> ()
  | l -> Fmt.fprintf out "@ [@[%a@]]" (Util.pp_list ~sep:"," pp_tag) l

module Kind = struct
  type t = kind

  let pp_parent_ out = function
    | `Name s -> Format.fprintf out "%s" s
    | `Theory s -> Format.fprintf out "theory(%s)" s

  let pp out k = match k with
    | Intro (src,R_goal) -> Format.fprintf out "goal %a" Src.pp src
    | Intro (src,R_lemma) -> Format.fprintf out "lemma %a" Src.pp src
    | Intro (src,R_assert) -> Src.pp out src
    | Intro (src, (R_def | R_decl)) -> Src.pp out src
    | Inference (rule,tags) ->
      Format.fprintf out "inf %a%a" Rule.pp rule pp_tags tags
    | Simplification (rule,tags) ->
      Format.fprintf out "simp %a%a" Rule.pp rule pp_tags tags
    | Esa rule ->
      Format.fprintf out "esa %a" Rule.pp rule
    | Trivial -> CCFormat.string out "trivial"
    | By_def id -> Format.fprintf out "by_def(%a)" ID.pp id
    | Define (id,src) -> Format.fprintf out "define(@[%a@ %a@])" ID.pp id Src.pp src

  let pp_tstp out (k,parents) =
    let pp_parents = Util.pp_list pp_parent_ in
    let pp_step status out (rule,parents) = match parents with
      | [] ->
        Format.fprintf out "inference(@[%a,@ [status(%s)]@])" Rule.pp rule status
      | _::_ ->
        Format.fprintf out "inference(@[%a,@ [status(%s)],@ [@[%a@]]@])"
          Rule.pp rule status pp_parents parents
    in
    begin match k with
      | Intro (src,(R_assert|R_goal|R_def|R_decl)) -> Src.pp_tstp out src
      | Inference (rule,_)
      | Simplification (rule,_) -> pp_step "thm" out (rule,parents)
      | Esa rule -> pp_step "esa" out (rule,parents)
      | Intro (_,R_lemma) -> Format.fprintf out "lemma"
      | Trivial -> assert(parents=[]); Format.fprintf out "trivial([status(thm)])"
      | By_def _ -> Format.fprintf out "by_def([status(thm)])"
      | Define _ -> Format.fprintf out "define([status(thm)])"
    end
end

module Result = struct
  type t = result
  type 'a tc = 'a result_tc

  let res_to_int_ = function (Res ({res_id; _}, _)) -> res_id

  type flavor =
    [ `Pure_bool
    | `Absurd_lits
    | `Proof_of_false
    | `Vanilla
    | `Def
    ]

  let compare a b = match a, b with
    | Res (r1,x1), Res (r2,x2) ->
      if r1.res_id <> r2.res_id
      then CCInt.compare r1.res_id r2.res_id
      else match r1.res_of_exn x1, r1.res_of_exn x2 with
        | Some y1, Some y2 -> r1.res_compare y1 y2
        | _ -> assert false (* same ID?? *)

  let equal a b = compare a b = 0

  let to_form ?(ctx=Term.Conv.create()) (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_to_form ~ctx x

  let to_form_subst ?(ctx=Term.Conv.create()) subst (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_to_form_subst ~ctx subst x

  let pp_in o out (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_pp_in o out x

  let pp = pp_in Output_format.normal

  let flavor (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_flavor x

  let name (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x ->
      begin match r.res_name with
        | None -> None
        | Some f -> Some (f x)
      end

  let n_ = ref 0
  let make_tc (type a)
      ~of_exn ~to_exn ~compare
      ~to_form
      ?(to_form_subst=fun ~ctx:_ _ _ -> assert false)
      ~pp_in
      ?name
      ?(is_stmt=false)
      ?(flavor=fun _ -> `Vanilla)
      () : a result_tc
    =
    let id = CCRef.incr_then_get n_ in
    { res_id=id;
      res_of_exn=of_exn;
      res_to_exn=to_exn;
      res_compare=compare;
      res_is_stmt=is_stmt;
      res_pp_in=pp_in;
      res_to_form=to_form;
      res_to_form_subst=to_form_subst;
      res_name=name;
      res_flavor=flavor;
    }

  let make tc x : t = Res (tc, tc.res_to_exn x)

  exception E_form of form

  type inst_subst = (term,term) Var.Subst.t

  let form_tc : form result_tc =
    make_tc
      ~of_exn:(function
        | E_form f -> Some f | _ -> None)
      ~to_exn:(fun f -> E_form f)
      ~to_form:(fun ~ctx:_ t -> t)
      ~compare:T.compare
      ~pp_in:TypedSTerm.pp_in
      ~flavor:(fun f -> if T.equal f F.false_ then `Proof_of_false else `Vanilla)
      ()

  let of_form = make form_tc
  let is_stmt (Res (r,_)) = r.res_is_stmt
end

let pp_parent out = function
  | P_of p -> Result.pp out p.result
  | P_subst (p,subst) ->
    Format.fprintf out "(@[instantiate `%a`@ :subst %a@])"
      Result.pp p.result Subst.Projection.pp subst

module Step = struct
  type t = step

  let equal p1 p2 = p1.id=p2.id
  let compare p1 p2 = CCInt.compare p1.id p2.id
  let hash p = p.id

  let kind p = p.kind
  let parents p = p.parents
  let infos p = p.infos

  let src p = match p.kind with
    | Intro (src,_) | Define (_,src) -> Some src
    | Trivial | By_def _ | Esa _ | Inference _ | Simplification _
      -> None

  let to_attrs p = match src p with
    | None -> []
    | Some src -> Src.to_attrs src

  let rule p = match p.kind with
    | Intro _
    | Trivial
    | By_def _
    | Define _ -> None
    | Esa rule
    | Simplification (rule,_)
    | Inference (rule,_)
      -> Some rule

  let is_assert p = match p.kind with Intro (_,R_assert) -> true | _ -> false
  let is_assert_like p = match p.kind with Intro (_,(R_assert|R_def|R_decl)) -> true | _ -> false
  let is_goal p = match p.kind with Intro (_,(R_goal|R_lemma)) -> true | _ -> false
  let is_trivial p = match p.kind with Trivial -> true | _ -> false
  let is_by_def p = match p.kind with By_def _ -> true | _ -> false

  let distance_to_goal p = p.dist_to_goal

  let get_id_ =
    let n = ref 0 in
    fun () -> CCRef.incr_then_get n

  let trivial = {id=get_id_(); parents=[]; kind=Trivial; dist_to_goal=None; infos=[]; }
  let by_def id = {id=get_id_(); parents=[]; kind=By_def id; dist_to_goal=None; infos=[]; }
  let intro src r =
    let dist_to_goal = match r with
      | R_goal | R_lemma -> Some 0 | _ -> None
    in
    {id=get_id_(); parents=[]; kind=Intro(src,r); dist_to_goal; infos=[]}
  let define id src parents =
    {id=get_id_(); parents; kind=Define (id,src); dist_to_goal=None; infos=[]; }
  let define_internal id parents = define id (Src.internal []) parents
  let lemma src =
    {id=get_id_(); parents=[]; kind=Intro(src,R_lemma); dist_to_goal=Some 0; infos=[]; }

  let combine_dist o p = match o, (Parent.proof p).step.dist_to_goal with
    | None, None -> None
    | (Some _ as res), None
    | None, (Some _ as res) -> res
    | Some x, Some y -> Some (min x y)

  let step_ ?(infos=[]) kind parents =
    (* distance to goal (0 if a goal itself) *)
    let dist_to_goal = match kind with
      | Intro (_,(R_goal | R_lemma)) -> Some 0
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

  let intro src r = step_ (Intro(src,r)) []

  let assert_ src = intro src R_assert

  let assert' ?loc ~file ~name () =
    let src = Src.from_file ?loc ~name file in
    assert_ src

  let goal src = intro src R_goal

  let goal' ?loc ~file ~name () =
    let src = Src.from_file ?loc ~name file in
    goal src

  let[@inline] dedup_tags (tgs:tag list) : tag list =
    CCList.sort_uniq ~cmp:Builtin.Tag.compare tgs

  let inference ?infos ?(tags=[]) ~rule parents =
    let tags = dedup_tags tags in
    step_ ?infos (Inference (rule,tags)) parents

  let simp ?infos ?(tags=[]) ~rule parents =
    let tags = dedup_tags tags in
    step_ ?infos (Simplification (rule,tags)) parents

  let esa ?infos ~rule parents =
    step_ ?infos (Esa rule) parents

  let pp_infos out = function
    | [] -> ()
    | l ->
      Format.fprintf out "@ %a" (Util.pp_list ~sep:" " UntypedAST.pp_attr) l

  let pp_parents out = function
    | [] -> ()
    | l ->
      Format.fprintf out "@ with @[<hv>%a@]"
        (Util.pp_list Result.pp)
        (List.map (fun p -> (Parent.proof p).result) @@ l)

  let pp out step = match kind step with
    | Intro (_,(R_assert|R_goal|R_def|R_decl)) ->
      Format.fprintf out "@[<hv2>%a@]%a" Kind.pp (kind step) pp_infos step.infos
    | Intro (_,R_lemma) -> Format.fprintf out "@[<2>lemma%a@]" pp_infos step.infos
    | Trivial -> Format.fprintf out "@[<2>trivial%a@]" pp_infos step.infos
    | By_def id -> Format.fprintf out "@[<2>by_def %a%a@]" ID.pp id pp_infos step.infos
    | Define (id,src) ->
      Format.fprintf out "@[<2>define %a@ %a%a%a@]"
        ID.pp id Src.pp src pp_parents (parents step) pp_infos step.infos
    | Inference _
    | Simplification _
    | Esa _ ->
      Format.fprintf out "@[<hv2>%a%a%a@]"
        Kind.pp (kind step) pp_parents (parents step) pp_infos step.infos
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

  let has_absurd_lits p = Result.flavor (result p) = `Absurd_lits

  let is_proof_of_false p = Result.flavor (result p) = `Proof_of_false

  let is_pure_bool p = Result.flavor (result p) = `Pure_bool
  let is_def p = Result.flavor (result p) = `Def

  let mk step res = {step; result=res}
  let mk_f step res = mk step (Result.of_form res)

  let mk_f_trivial = mk_f Step.trivial
  let mk_f_by_def id f = mk_f (Step.by_def id) f

  let mk_f_inference ~rule f parents =
    let step = Step.inference ~rule parents in
    mk_f step f

  let mk_f_simp ~rule f parents =
    let step = Step.simp ~rule parents in
    mk_f step f

  let mk_f_esa ~rule f parents =
    let step = Step.esa ~rule parents in
    mk_f step f

  let adapt p r = { p with result=r; }

  let adapt_f p f = adapt p (Result.of_form f)

  let name_gen_ = ref 0

  (* retrieve the name, or create a new one on the fly *)
  let name ~namespace (p:t) : string =
    (* look if the result is a named thing from the input, otherwise
       generate a fresh one from the namespace *)
    begin match Result.name (result p) with
      | Some s -> s
      | None ->
        try Tbl.find namespace p
        with Not_found ->
          let s = Printf.sprintf "'%d'" (Tbl.length namespace) in
          Tbl.add namespace p s;
          s
    end

  (** {2 Conversion to a graph of proofs} *)

  (** Get a graph of the proof *)
  let as_graph : (t, rule * Subst.Projection.t option * infos) CCGraph.t =
    CCGraph.make
      (fun p ->
         let st = step p in
         let rule = match Step.rule st with
           | None -> ""
           | Some rule -> rule
         in
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

  let pp_notrec1 out p =
    Format.fprintf out "@[<hv>%a by %a@ from [@[<v>%a@]]@]"
      pp_result_of p Kind.pp (Step.kind @@ step p)
      (Util.pp_list pp_parent) p.step.parents

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
    let namespace = Tbl.create 8 in
    Format.fprintf out "@[<v>";
    traverse ~order:`DFS proof
      (fun p ->
         let p_name = name ~namespace p in
         let parents =
           List.map (fun p -> `Name (name ~namespace @@ Parent.proof p))
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
         if Result.is_stmt (result p) then (
           Format.fprintf out "%a@," (Result.pp_in Output_format.tptp) (result p)
         ) else (
           Format.fprintf out "tff(@[%s, %s,@ @[%a@],@ @[%a@]%a@]).@,"
             p_name role (Result.pp_in Output_format.tptp) (result p)
             Kind.pp_tstp (Step.kind @@ step p,parents) pp_infos infos
         ));
    Format.fprintf out "@]";
    ()

  let pp_zf out proof =
    let module UA = UntypedAST.A in
    Format.fprintf out "@[<v>";
    let namespace = Tbl.create 8 in
    traverse ~order:`DFS proof
      (fun p ->
         let p_name = name ~namespace p in
         let parents =
           List.map (fun p -> name ~namespace @@ Parent.proof p)
             (Step.parents @@ step p)
         in
         let mk_status r = UA.app "status" [UA.quoted r] in
         let info_name =
           UA.(app "name" [str p_name])
         and info_from =
           if parents=[] then []
           else (
             [UA.(app "from" [list (List.map str parents)])]
           )
         and info_rule = match Step.rule (step p) with
           | Some r -> [UA.(app "rule" [quoted r])]
           | None -> []
         and info_status = match Step.kind (step p) with
           | Inference _ | Simplification _ -> [mk_status "inference"]
           | Esa _ -> [mk_status "equisatisfiable"]
           | Intro (src,R_lemma) -> mk_status "lemma" :: Src.to_attrs src
           | Intro (src,R_goal) -> mk_status "goal" :: Src.to_attrs src
           | Intro (src,R_assert) -> mk_status "assert" :: Src.to_attrs src
           | Intro (src,R_def) -> mk_status "def" :: Src.to_attrs src
           | Intro (src,R_decl) -> mk_status "decl" :: Src.to_attrs src
           | Trivial -> [mk_status "trivial"]
           | By_def _ | Define _ -> []
         in
         let pp_infos = UntypedAST.pp_attrs_zf in
         let infos =
           info_name :: info_from @ info_rule @ info_status @ (Step.infos p.step)
         in
         if Result.is_stmt (result p) then (
           Format.fprintf out "%a@," (Result.pp_in Output_format.zf) (result p)
         ) else (
           Format.fprintf out "@[<2>assert%a@ %a@].@,"
             pp_infos infos (Result.pp_in Output_format.zf) (result p)
         ));
    Format.fprintf out "@]";
    ()

  (** Prints the proof according to the given input switch *)
  let pp_in o out proof = match o with
    | Output_format.O_none -> Util.debug ~section 1 "proof printing disabled"
    | Output_format.O_tptp -> pp_tstp out proof
    | Output_format.O_normal -> pp_normal out proof
    | Output_format.O_zf -> pp_zf out proof

  let _pp_list_str = Util.pp_list CCFormat.string

  let _to_str_escape fmt =
    Util.ksprintf_noc ~f:Util.escape_dot fmt

  let pp_dot_seq ~name out seq =
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
        else if is_def p then `Color "navajowhite" :: shape :: attrs
        else if Step.is_goal @@ step p then `Color "green" :: shape :: attrs
        else if Step.is_trivial @@ step p then `Color "cyan" :: shape :: attrs
        else if Step.is_by_def @@ step p then `Color "navajowhite" :: shape :: attrs
        else if Step.is_assert_like @@ step p then `Color "yellow" :: shape :: attrs
        else shape :: attrs
      )
      ~attrs_e:(fun (r,s,infos) ->
        let pp_subst out s =
          if not (Subst.is_empty @@ Subst.Projection.subst s) then (
            Format.fprintf out "@,%a" Subst.Projection.pp s
          )
        in
        let label =
          if s=None && infos=[] then Rule.name r
          else (
            _to_str_escape "@[<v>%s%a%a@]@."
              (Rule.name r) (CCFormat.some pp_subst) s Step.pp_infos infos
          )
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
end
