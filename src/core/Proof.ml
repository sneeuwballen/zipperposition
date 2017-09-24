
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

module Loc = ParseLocation
module T = TypedSTerm
module F = T.Form
module UA = UntypedAST

type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

let section = Util.Section.make "proof"

type rule = string

type check = [`No_check | `Check | `Check_with of form list]

type attrs = UntypedAST.attrs

type info = UntypedAST.attr

type infos = info list

type kind =
  | Intro of source * role
  | Inference of rule * check
  | Simplification of rule * check
  | Esa of rule * check
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
  res_pp_in: Output_format.t -> 'a CCFormat.printer;
  res_to_form: ctx:Term.Conv.ctx -> 'a -> TypedSTerm.Form.t;
  res_apply_subst: (Subst.t -> 'a Scoped.t -> 'a) option;
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

  let from_file ?loc ?name ?(attrs=[]) file : t =
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

  let rec pp_tstp out src = match view src with
    | Internal _ -> ()
    | From_file (src,_) ->
      let file = src.file in
      begin match src.name with
        | None -> Format.fprintf out "file('%s')" file
        | Some name -> Format.fprintf out "file(@['%s',@ '%s'@])" file name
      end

  let rec pp out src = match view src with
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
          | Some n -> app "file" [quoted f.file; app "name" [quoted n]] :: attrs
        end
    end
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
    | Intro (src,R_goal) -> Format.fprintf out "goal %a" Src.pp src
    | Intro (src,R_lemma) -> Format.fprintf out "lemma %a" Src.pp src
    | Intro (src,R_assert) -> Src.pp out src
    | Intro (src, (R_def | R_decl)) -> Src.pp out src
    | Inference (rule,_) ->
      Format.fprintf out "inf %a" Rule.pp rule
    | Simplification (rule,_) ->
      Format.fprintf out "simp %a" Rule.pp rule
    | Esa (rule,_) ->
      Format.fprintf out "esa %a" Rule.pp rule
    | Trivial -> CCFormat.string out "trivial"
    | By_def id -> Format.fprintf out "by_def(%a)" ID.pp id
    | Define (id,src) -> Format.fprintf out "define(@[%a@ %a@])" ID.pp id Src.pp src

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
      | Intro (src,(R_assert|R_goal|R_def|R_decl)) -> Src.pp_tstp out src
      | Inference (rule,_)
      | Simplification (rule,_) -> pp_step "thm" out (rule,parents)
      | Esa (rule,_) -> pp_step "esa" out (rule,parents)
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

  let pp_in o out (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_pp_in o out x

  let pp = pp_in Output_format.normal

  let to_form ?(ctx=Term.Conv.create()) (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_to_form ~ctx x

  let flavor (Res (r,x)) = match r.res_of_exn x with
    | None -> assert false
    | Some x -> r.res_flavor x

  let apply_subst subst (Res (r,x),sc) : t = match r.res_of_exn x with
    | None -> assert false
    | Some y ->
      let y = match r.res_apply_subst with
        | None -> y
        | Some f -> f subst (y,sc)
      in
      Res (r, r.res_to_exn y)

  let n_ = ref 0
  let make_tc (type a)
      ~of_exn ~to_exn ~compare ~pp_in
      ~to_form
      ?apply_subst
      ?(flavor=fun _ -> `Vanilla)
      () : a result_tc
    =
    let id = CCRef.incr_then_get n_ in
    { res_id=id;
      res_of_exn=of_exn;
      res_to_exn=to_exn;
      res_compare=compare;
      res_pp_in=pp_in;
      res_apply_subst=apply_subst;
      res_to_form=to_form;
      res_flavor=flavor;
    }

  let make tc x : t = Res (tc, tc.res_to_exn x)

  exception E_form of form

  let form_tc : form result_tc =
    make_tc
      ~of_exn:(function
        | E_form f -> Some f | _ -> None)
      ~to_exn:(fun f -> E_form f)
      ~to_form:(fun ~ctx:_ t -> t)
      ~compare:T.compare
      ~flavor:(fun f -> if T.equal f F.false_ then `Proof_of_false else `Vanilla)
      ~pp_in:T.pp_in
      ()

  let of_form = make form_tc

  (* FIXME
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
     *)
end

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
    | Trivial | By_def _ | Esa (_,_) | Inference _ | Simplification _
      -> None

  let to_attrs p = match src p with
    | None -> []
    | Some src -> Src.to_attrs src

  let rule p = match p.kind with
    | Intro _
    | Trivial
    | By_def _
    | Define _ -> None
    | Esa (rule,_)
    | Simplification (rule,_)
    | Inference (rule,_)
      -> Some rule

  let is_assert p = match p.kind with Intro (_,R_assert) -> true | _ -> false
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
    | Intro (_,(R_assert|R_goal|R_def|R_decl)) ->
      Format.fprintf out "@[<hv2>%a@]%a@," Kind.pp (kind step) pp_infos step.infos
    | Intro (_,R_lemma) -> Format.fprintf out "@[<2>lemma%a@]" pp_infos step.infos
    | Trivial -> Format.fprintf out "@[<2>trivial%a@]" pp_infos step.infos
    | By_def id -> Format.fprintf out "@[<2>by_def %a%a@]" ID.pp id pp_infos step.infos
    | Define (id,src) ->
      Format.fprintf out "@[<2>define %a@ %a%a@]" ID.pp id Src.pp src pp_infos step.infos
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

  let has_absurd_lits p = Result.flavor (result p) = `Absurd_lits

  let is_proof_of_false p = Result.flavor (result p) = `Proof_of_false

  let is_pure_bool p = Result.flavor (result p) = `Pure_bool
  let is_def p = Result.flavor (result p) = `Def

  let mk step res = {step; result=res}
  let mk_f step res = mk step (Result.of_form res)

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

  let adapt p r = { p with result=r; }

  let adapt_f p f = adapt p (Result.of_form f)

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
    let namespace = Tbl.create 8 in
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
         Format.fprintf out "@[<2>tff(%d, %s,@ @[%a@],@ @[%a@]%a).@]@,"
           name role (Result.pp_in Output_format.tptp) (result p)
           Kind.pp_tstp (Step.kind @@ step p,parents) pp_infos infos);
    Format.fprintf out "@]";
    ()

           (* FIXME: readapt this (custom fun in res_tc?)
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
           *)

  let pp_zf out proof =
    let module UA = UntypedAST.A in
    let namespace = Tbl.create 8 in
    Format.fprintf out "@[<v>";
    traverse ~order:`DFS proof
      (fun p ->
         let name = get_name ~namespace p in
         let parents =
           List.map (fun p -> get_name ~namespace @@ Parent.proof p)
             (Step.parents @@ step p)
         in
         let str_of_name s = CCFormat.sprintf "'%d'" s in
         let mk_status r = UA.app "status" [UA.quoted r] in
         let info_name =
           UA.(app "name" [str (str_of_name name)])
         and info_from =
           if parents=[] then []
           else (
             let l = List.map str_of_name parents in
             [UA.(app "from" [list (List.map str l)])]
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
         Format.fprintf out "@[<2>assert%a@ %a@].@,"
           pp_infos infos (Result.pp_in Output_format.zf) (result p));
    Format.fprintf out "@]";
    ()

  (* FIXME:
         begin match result p with
           | Form f ->
             Format.fprintf out "@[<2>assert%a@ %a@].@,"
               pp_infos infos TypedSTerm.ZF.pp f
           | BoolClause c ->
             Format.fprintf out "@[<2>assert%a@ %a@].@,"
               pp_infos infos (Util.pp_list ~sep:" || " BBox.pp_zf) c
           | Clause c ->
             Format.fprintf out "@[<2>assert%a@ %a@].@,"
               pp_infos infos SClause.pp_zf c
           | Stmt stmt ->
             let module T = TypedSTerm in
             Statement.ZF.pp T.ZF.pp T.ZF.pp T.ZF.pp out stmt
           | C_stmt stmt ->
             let pp_t = Term.ZF.pp in
             let pp_c = Util.pp_list ~sep:" || " (SLiteral.ZF.pp pp_t) in
             Statement.ZF.pp pp_c pp_t Type.ZF.pp out stmt
         end);
     *)

  (** Prints the proof according to the given input switch *)
  let pp_in o out proof = match o with
    | Output_format.O_none -> Util.debug ~section 1 "proof printing disabled"
    | Output_format.O_tptp -> pp_tstp out proof
    | Output_format.O_normal -> pp_normal out proof
    | Output_format.O_zf -> pp_zf out proof

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
        else if is_def p then `Color "navajowhite" :: shape :: attrs
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
          else _to_str_escape "@[<v>%s%a%a@]@."
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
      let res = Result.to_form ~ctx (result p) in
      let parents =
        List.map (conv_parent ~ctx) (Step.parents @@ step p)
      in
      begin match Step.kind @@ step p with
        | Inference (name,c)
        | Simplification (name,c) -> LLProof.inference c res name parents
        | Esa (name,c) -> LLProof.esa c res name parents
        | Trivial -> LLProof.trivial res
        | By_def id -> LLProof.by_def id res
        | Define (id,_) -> LLProof.define id res
        | Intro (_,R_assert) -> LLProof.assert_ res
        | Intro (_,R_goal) -> LLProof.assert_ res
        | Intro (_,(R_lemma|R_def|R_decl)) -> LLProof.trivial res
      end
    and conv_parent ~ctx (p:Parent.t): LLProof.t = match p with
      | P_of p -> conv p
      | P_subst (p,sc_p,subst) ->
        let p' = conv_parent ~ctx p in
        (* build instance of result *)
        let res = result @@ Parent.proof p in
        let res_subst =
          Result.apply_subst subst (res,sc_p)
          |> Result.to_form ~ctx
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
end
