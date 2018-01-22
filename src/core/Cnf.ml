
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Reduction to CNF and simplifications} *)

module T = TypedSTerm
module F = T.Form
module Stmt = Statement
module Fmt = CCFormat

let prof_estimate = Util.mk_profiler "cnf.estimate_num_clauses"
let prof_simplify_rename = Util.mk_profiler "cnf.simplify_rename"
let prof_flatten = Util.mk_profiler "cnf.flatten"
let prof_to_cnf = Util.mk_profiler "cnf.distribute"
let prof_miniscope = Util.mk_profiler "cnf.miniscope"
let prof_skolemize = Util.mk_profiler "cnf.skolemize"

let section = Util.Section.make "cnf"

type term = T.t
type type_ = T.t
type form = F.t
type lit = term SLiteral.t

exception Error of string

exception NotCNF of form

let () = Printexc.register_printer
    (function
      | Error msg -> Some (Fmt.sprintf "@[<2>error in CNF:@ %s@]" msg)
      | NotCNF f -> Some (Fmt.sprintf "@[<2>error:@ @[%a@]@ is not in CNF@]" T.pp f)
      | _ -> None)

let error_ msg = raise (Error msg)
let errorf_ msg = Fmt.ksprintf msg ~f:error_
let not_cnf_ f = raise (NotCNF f)

type clause = lit list

let pp_clause out =
  Format.fprintf out "@[<2>%a@]" (Util.pp_list ~sep:" ∨ " (SLiteral.pp T.pp))

let clause_to_fo ?(ctx=Term.Conv.create()) c =
  List.map (SLiteral.map ~f:(Term.Conv.of_simple_term_exn ctx)) c

let as_lit = SLiteral.of_form
let as_lit_opt t = try Some (as_lit t) with _ -> None

(* check whether the formula is already in CNF *)
let as_clause f = match F.view f with
  | F.Or l -> List.map as_lit l
  | F.Not _
  | F.True
  | F.False
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> [as_lit f]
  | F.And _
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> not_cnf_ f

let as_cnf f = match F.view f with
  | F.Or _ -> [as_clause f]
  | F.Not _
  | F.True
  | F.False
  | F.Atom _
  | F.Neq _
  | F.Eq _ -> [[as_lit f]]
  | F.And l -> List.map as_clause l
  | F.Equiv _
  | F.Xor _
  | F.Imply _
  | F.Forall _
  | F.Exists _ -> not_cnf_ f

let is_clause f =
  try ignore (as_clause f); true
  with NotCNF _ | SLiteral.NotALit _ -> false

let is_cnf f =
  try ignore (as_cnf f); true
  with NotCNF _ | SLiteral.NotALit _ -> false

module Flatten = struct
  type 'a t = T.Subst.t -> (T.Subst.t * 'a) Sequence.t

  let empty : 'a t = fun _ -> Sequence.empty

  let return
    : type a. a -> a t
    = fun x subst -> Sequence.return (subst, x)

  let (>>=)
    : type a b. a t -> (a -> b t) -> b t
    = fun seq f subst yield ->
      seq subst (fun (subst,x) ->
        f x subst (fun (subst',y) ->
          yield (T.Subst.merge subst subst',y)))

  let (>|=)
    : type a b. a t -> (a -> b) -> b t
    = fun seq f subst yield ->
      seq subst (fun (subst,x) ->
        yield (subst,f x))

  let (<*>)
    : ('a -> 'b) t -> 'a t -> 'b t
    = fun f x ->
      f >>= fun f ->
      x >|= fun x -> f x

  let (<$>) f x = x >|= f ?loc:None

  let add_subst
    : T.Subst.t -> unit t
    = fun sigma subst ->
      Sequence.return (T.Subst.merge subst sigma, ())

  let get_subst : T.Subst.t t
    = fun subst -> Sequence.return (subst, subst)

  (* non deterministic choice *)
  let (<+>)
    : type a. a t -> a t -> a t
    = fun x y subst ->
      Sequence.append (x subst) (y subst)

  let rec map_m f l = match l with
    | [] -> return []
    | x :: tail ->
      f x >>= fun x ->
      map_m f tail >|= fun tail -> x::tail

  let map_sliteral
    : ('a -> 'b t) -> 'a SLiteral.t -> 'b SLiteral.t t
    = fun f lit -> match lit with
      | SLiteral.True -> return SLiteral.true_
      | SLiteral.False -> return SLiteral.false_
      | SLiteral.Atom (t, b) ->
        f t >|= fun t -> SLiteral.atom t b
      | SLiteral.Eq (a,b) -> return SLiteral.eq <*> f a <*> f b
      | SLiteral.Neq (a,b) -> return SLiteral.neq <*> f a <*> f b

  let rec fold_m f acc l = match l with
    | [] -> return acc
    | [x] -> f acc x
    | x :: tail ->
      f acc x >>= fun acc -> fold_m f acc tail

  let choice_l
    : 'a t list -> 'a t
    = fun l subst ->
      Sequence.flat_map (fun x -> x subst) (Sequence.of_list l)

  let of_list
    : 'a list -> 'a t
    = fun l subst ->
      Sequence.of_list l |> Sequence.map (fun x->subst,x)

  let to_list
    : 'a t -> (T.Subst.t * 'a) list
    = fun seq ->
      seq T.Subst.empty |> Sequence.to_rev_list

  let to_list' : 'a t -> 'a list
    = fun seq -> seq T.Subst.empty |> Sequence.map snd |> Sequence.to_rev_list

  let apply_subst_vars_ subst vars =
    List.map
      (fun v -> try Var.Subst.find_exn subst v with Not_found -> T.var v)
      vars

  type position =
    | Pos_toplevel (* outside, as a formula *)
    | Pos_inner (* inside a term *)

  (* put type variables first *)
  let ty_vars_first (l:_ Var.t list): _ Var.t list =
    let ty_vars, other_vars =
      List.partition (fun v -> T.Ty.is_tType (Var.ty v)) l
    in
    ty_vars @ other_vars

  let pp_rules =
    Fmt.(Util.pp_list Dump.(pair (list T.pp_inner |> hovbox) T.pp) |> hovbox)

  (* [t=u], but one of them is a lambda/function type. Add/create new variables
     [vars] and turn this into [t vars = u vars], modulo beta.
     @return [vars, t vars, u vars] *)
  let complete_eq t u =
    let ty_vars, ty_args, ty_ret = T.Ty.unfold (T.ty_exn t) in
    assert (List.length ty_vars + List.length ty_args > 0);
    (* unfold t and u *)
    let vars_t, t = T.unfold_fun t in
    let vars_u, u = T.unfold_fun u in
    (* variable names, for re-use *)
    let names =
      if List.length vars_t > List.length vars_u
      then List.map Var.name vars_t
      else List.map Var.name vars_u
    in
    (* make variables for full application *)
    let vars =
      List.mapi
        (fun i ty ->
           let name = try List.nth names i with _ -> Printf.sprintf "x_%d" i in
           Var.of_string ~ty name)
        ty_args
    in
    let mk_args_subst vars' =
      let n = List.length vars' in
      List.map T.var (CCList.drop n vars),
      Var.Subst.of_list
        (List.combine vars' (CCList.take n vars |> List.map T.var))
    in
    let args_t, subst_t = mk_args_subst vars_t in
    let args_u, subst_u = mk_args_subst vars_u in
    (* apply and normalize *)
    let t = T.app_whnf ~ty:ty_ret (T.Subst.eval subst_t t) args_t in
    let u = T.app_whnf ~ty:ty_ret (T.Subst.eval subst_u u) args_u in
    ty_vars @ vars, t, u

  (* conversion of terms can yield several possible terms, by
     eliminating if and match
     @param vars the variables that can be replaced in the context
     @param of_ the ID being defined, if any
  *)
  let flatten_rec ?of_ (ctx:Skolem.ctx) stmt (pos:position) (vars:type_ Var.t list)(t:term): T.t t =
    (* how to name intermediate subterms? *)
    let mk_pat what = match of_ with
      | None -> what ^  "_"
      | Some id -> Fmt.sprintf "%s_%s_" (ID.name id) what
    in
    let rec aux pos vars t = match T.view t with
      | T.AppBuiltin (Builtin.True, []) -> return F.true_
      | T.AppBuiltin (Builtin.False, []) -> return F.false_
      | T.Var v ->
        (* look [v] up in subst *)
        get_subst >>= fun subst ->
        begin match T.Subst.find subst v with
          | None -> return t
          | Some t' -> aux pos vars t'
        end
      | T.Const _ -> return t
      | T.App (f,l) ->
        T.app ~ty:(T.ty_exn t)
        <$> aux Pos_inner vars f
        <*> map_m (aux Pos_inner vars) l
      | T.Ite (a,b,c) ->
        begin match T.view a with
          | T.Var x when List.exists (Var.equal x) vars ->
            (* substitute [x] with [true] and [false] in either branch *)
            begin
              (add_subst (Var.Subst.singleton x F.true_) >>= fun () -> aux pos vars b)
              <+>
                (add_subst (Var.Subst.singleton x F.false_) >>= fun () -> aux pos vars c)
            end
          | _ ->
            (* give a name to [if a b c] *)
            let closure = T.free_vars_l [b;c] |> ty_vars_first in
            let cases_true =
              aux pos vars b >>= fun b ->
              get_subst >|= fun subst ->
              apply_subst_vars_ subst closure @ [F.true_], T.Subst.eval subst b
            and cases_false =
              aux pos vars c >>= fun c ->
              get_subst >|= fun subst ->
              apply_subst_vars_ subst closure @ [F.false_], T.Subst.eval subst c
            in
            let rules = to_list' (cases_true <+> cases_false) in
            let def =
              Skolem.define_term ~ctx rules ~pattern:(mk_pat "ite")
                ~parents:[Stmt.as_proof_i stmt |> Proof.Parent.from]
            in
            aux Pos_toplevel vars a >|= fun a ->
            T.app ~ty:(T.ty_exn b)
              (T.const def.Skolem.td_id ~ty:def.Skolem.td_ty)
              (List.map T.var closure @ [a])
        end
      | T.Let (l,u) ->
        (* add let-bound terms to substitution without processing them.
           They will be processed differently at every place the
           variables are used *)
        let subst =
          List.fold_left
            (fun s (v,t) ->
               let t = T.Subst.eval s t in
               T.Subst.add s v t)
            T.Subst.empty l
        in
        add_subst subst >>= fun () ->
        aux pos vars u
      | T.Match (u,l) ->
        begin match T.view u with
          | T.Var x when List.exists (Var.equal x) vars ->
            (* will replace [x] by each constructor *)
            of_list l >>= fun (c,c_vars,rhs) ->
            (* the term [c vars] *)
            let case =
              let ty = T.ty_exn u in
              T.app ~ty
                (T.const_of_cstor c)
                (c.T.cstor_args @ List.map T.var c_vars)
            in
            (* bind [x = c vars] *)
            let subst = T.Subst.add T.Subst.empty x case in
            add_subst subst >>= fun () ->
            (* directly replace by [rhs]. [c_vars] can be replaced themselves. *)
            aux pos (c_vars@vars) rhs
          | _ ->
            (* give a name to the match *)
            (* first, compute closure variables, i.e. variables that are free
               in the branches. *)
            let closure =
              l
              |> List.rev_map
                (fun (_,match_vars,rhs) ->
                   (* match variables are not free *)
                   T.Form.forall_l match_vars rhs)
              |> T.free_vars_l
              |> ty_vars_first
            in
            let cases =
              of_list l >>= fun (cstor,c_vars,rhs) ->
              aux pos (c_vars@vars) rhs >>= fun rhs ->
              get_subst >|= fun subst ->
              let case =
                T.app ~ty:(T.ty_exn u)
                  (T.const_of_cstor cstor)
                  (cstor.T.cstor_args @ apply_subst_vars_ subst c_vars)
              in
              apply_subst_vars_ subst closure @ [case], T.Subst.eval subst rhs
            in
            let rules = to_list' cases in
            Util.debugf ~section 5 "(@[define_match@ :term %a@ :rules %a@])"
              (fun k->k T.pp t pp_rules rules);
            let def =
              Skolem.define_term ~ctx rules ~pattern:(mk_pat "match")
                ~parents:[Stmt.as_proof_i stmt |> Proof.Parent.from]
            in
            (* now apply definition to [u] *)
            aux Pos_inner vars u >|= fun u ->
            T.app ~ty:(T.ty_exn t)
              (T.const def.Skolem.td_id ~ty:def.Skolem.td_ty)
              (List.map T.var closure @ [u])
        end
      | T.AppBuiltin (Builtin.Greater, [ty;a;b]) when T.equal T.Ty.rat (T.ty_exn a) ->
        aux pos vars (T.app_builtin ~ty:T.Ty.prop Builtin.Less [ty;b;a])
      | T.AppBuiltin (Builtin.Greatereq, [ty;a;b]) when T.equal T.Ty.rat (T.ty_exn a) ->
        aux pos vars (T.app_builtin ~ty:T.Ty.prop Builtin.Lesseq [ty;b;a])
      | T.AppBuiltin (Builtin.Neq, [a;b]) when T.equal T.Ty.rat (T.ty_exn a) ->
        (* rat: a!=b -> a<b ∨ a>b *)
        aux Pos_toplevel vars a >>= fun a ->
        aux Pos_toplevel vars b >|= fun b ->
        let f = T.Form.or_
            [ T.app_builtin ~ty:T.Ty.prop Builtin.Less [a; b];
              T.app_builtin ~ty:T.Ty.prop Builtin.Less [b; a];
            ]
        in aux_maybe_define pos f
      | T.AppBuiltin (Builtin.Eq, [a;b]) when T.is_fun a || T.is_fun b ->
        (* turn [f = λx. t] into [∀x. f x=t] *)
        let vars_forall, a, b = complete_eq a b in
        let t' = F.forall_l vars_forall (F.eq_or_equiv a b) in
        Util.debugf ~section 5 "(@[<2>rewrite-eq@ `%a`@ :into `%a`@])"
          (fun k->k T.pp t T.pp t');
        assert (vars_forall<>[]);
        aux pos vars t'
      | T.AppBuiltin (Builtin.Eq, [a;b]) ->
        (F.eq <$> aux Pos_toplevel vars a <*> aux Pos_toplevel vars b)
        >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Neq, [a;b]) when T.is_fun a || T.is_fun b ->
        (* turn [f ≠ λx. t] into [∃x. f x≠t] *)
        let vars_exist, a, b = complete_eq a b in
        let t' = F.exists_l vars_exist (F.neq_or_xor a b) in
        Util.debugf ~section 5 "(@[<2>rewrite-eq@ `%a`@ :into `%a`@])"
          (fun k->k T.pp t T.pp t');
        assert (vars_exist<>[]);
        aux pos vars t'
      | T.AppBuiltin (Builtin.Neq, [a;b]) ->
        (F.neq <$> aux Pos_toplevel vars a <*> aux Pos_toplevel vars b)
        >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Imply, [a;b]) ->
        (F.imply <$> aux Pos_toplevel vars a <*> aux Pos_toplevel vars b)
        >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Equiv, [a;b]) ->
        (F.equiv <$> aux Pos_toplevel vars a <*> aux Pos_toplevel vars b)
        >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Xor, [a;b]) ->
        (F.xor <$> aux Pos_toplevel vars a <*> aux Pos_toplevel vars b)
        >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.And, l) ->
        (F.and_ <$> map_m (aux Pos_toplevel vars) l) >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Or, l) ->
        (F.or_ <$> map_m (aux Pos_toplevel vars) l) >|= aux_maybe_define pos
      | T.AppBuiltin (Builtin.Not, [a]) ->
        (F.not_ <$> aux Pos_toplevel vars a) >|= aux_maybe_define pos
      | T.AppBuiltin (b, l) ->
        return (T.app_builtin ~ty:(T.ty_exn t) b) <*> map_m (aux Pos_inner vars) l
      | T.Bind (Binder.Forall,var,body) ->
        (aux Pos_toplevel vars body >|= F.forall var) >|= aux_maybe_define pos
      | T.Bind (Binder.Exists,var,body) ->
        (aux Pos_toplevel vars body >|= F.exists var) >|= aux_maybe_define pos
      | T.Bind (Binder.Lambda, _, _) ->
        (* lambda-lifting *)
        let fun_vars, body = T.unfold_fun t in
        assert (fun_vars <> []);
        (* flatten body (but [fun_vars] are protected) *)
        aux Pos_inner vars body >|= fun body ->
        T.fun_l fun_vars body
      | T.Bind (Binder.ForallTy,_,_)
      | T.Multiset _
      | T.Record _
      | T.Meta _ -> assert false
    (* if [pos = Pos_inner], introduce a name for formula [f] *)
    and aux_maybe_define pos f =
      assert (T.Ty.is_prop (T.ty_exn f));
      begin match pos with
        | Pos_toplevel -> f
        | Pos_inner ->
          let def =
            Skolem.define_form f ~ctx ~rw_rules:true ~polarity:`Both
              ~parents:[Stmt.as_proof_i stmt |> Proof.Parent.from]
              ~pattern:(mk_pat "form")
          in
          def.Skolem.proxy
      end
    in
    Util.debugf ~section 5 "@[<2>flatten_rec@ `@[%a@]`@ vars: (@[%a@])@]"
      (fun k->k T.pp t (Util.pp_list Var.pp_fullc) vars);
    aux pos vars t

  let flatten_rec_l ?of_ ctx stmt pos vars l =
    map_m (flatten_rec ?of_ ctx stmt pos vars) l
end

(* miniscoping (push quantifiers as deep as possible in the formula) *)
let miniscope ?(distribute_exists=false) f =
  Util.enter_prof prof_miniscope;
  (* recursive miniscoping *)
  let rec miniscope f = match F.view f with
    | F.Forall (var, f') ->
      begin match F.view f' with
        | F.And l ->
          (* forall x (and l) -> and (forall x f' \ f' in l) *)
          let l = List.map miniscope l in
          let with_v, without_v = List.partition (T.var_occurs ~var) l in
          F.and_ (List.map (F.forall var) with_v @ without_v)
        | F.Or l ->
          let l = List.map miniscope l in
          let with_v, without_v = List.partition (T.var_occurs ~var) l in
          F.or_ (F.forall var (F.or_ with_v) :: without_v)
        | _ -> F.forall var (miniscope f')
      end
    | F.Exists (var, f') ->
      begin match F.view f' with
        | F.And l ->
          let l = List.map miniscope l in
          let with_v, without_v = List.partition (T.var_occurs ~var) l in
          F.and_
            (F.exists var (F.and_ with_v) :: without_v)
        | F.Or l ->
          let l = List.map miniscope l in
          let with_v, without_v = List.partition (T.var_occurs ~var) l in
          (* see whether we push the existential into the formulas [with_v], in
              which the variable occurs, or whether we keep it outside *)
          let with_v = if distribute_exists
            then List.map (F.exists var) with_v
            else [F.exists var (F.or_ with_v)]
          in
          F.or_ (with_v @ without_v)
        | _ -> F.exists var (miniscope f')
      end
    | F.And l -> F.and_ (List.map miniscope l)
    | F.Or l -> F.or_ (List.map miniscope l)
    | F.Imply (f1, f2) -> F.imply (miniscope f1) (miniscope f2)
    | F.Equiv (f1, f2) -> F.equiv (miniscope f1) (miniscope f2)
    | F.Xor (f1, f2) -> F.xor (miniscope f1) (miniscope f2)
    | F.Not f' -> F.not_ (miniscope f')
    | F.True
    | F.False
    | F.Neq _
    | F.Eq _
    | F.Atom _ -> f
  in
  let res = miniscope f in
  Util.exit_prof prof_miniscope;
  res

(* negation normal form (also remove equivalence and implications). *)
let rec nnf f =
  Util.debugf ~section 5 "@[<2>nnf of@ `@[%a@]`@]" (fun k->k T.pp f);
  match F.view f with
    | F.Atom _
    | F.Neq _
    | F.Eq _
    | F.True
    | F.False -> f
    | F.Not f' ->
      begin match F.view f' with
        | F.Not f'' -> nnf f''
        | F.Neq (a,b) -> F.eq a b
        | F.Eq (a,b) -> F.neq a b
        | F.And l ->
          F.or_ (List.map (fun f -> nnf (F.not_ f)) l)
        | F.Or l ->
          F.and_ (List.map (fun f -> nnf (F.not_ f)) l)
        | F.Xor (a,b) ->
          nnf (F.equiv a b)
        | F.Equiv (a,b) ->
          nnf (F.xor a b)
        | F.Imply (a,b) -> (* not (a=>b)  is a and not b *)
          nnf (F.and_ [a; F.not_ b])
        | F.Forall (var, f'') ->
          F.exists var (nnf (F.not_ f''))
        | F.Exists (var, f'') ->
          F.forall var (nnf (F.not_ f''))
        | F.True -> F.false_
        | F.False -> F.true_
        | F.Atom _ -> f
      end
    | F.And l -> F.and_ (List.map nnf l)
    | F.Or l -> F.or_ (List.map nnf l)
    | F.Imply (f1, f2) ->
      nnf (F.or_ [ (F.not_ f1); f2 ])
    | F.Equiv(f1,f2) ->
      (* equivalence with positive polarity *)
      nnf (F.and_
          [ F.imply f1 f2; F.imply f2 f1 ])
    | F.Xor (f1,f2) ->
      (* equivalence with negative polarity *)
      nnf (F.and_
          [ F.or_ [f1; f2]; F.or_ [F.not_ f1; F.not_ f2] ])
    | F.Forall (var,f') -> F.forall var (nnf f')
    | F.Exists (var,f') -> F.exists var (nnf f')

let skolemize ~ctx f =
  Util.enter_prof prof_skolemize;
  let rec skolemize subst f = match F.view f with
    | F.And l -> F.and_ (List.map (skolemize subst) l)
    | F.Or l -> F.or_ (List.map (skolemize subst) l)
    | F.Not f' -> F.not_ (skolemize subst f')
    | F.Xor _
    | F.Imply _
    | F.Equiv _ -> error_ "can only skolemize a NNF formula"
    | F.Atom _
    | F.Eq _
    | F.Neq _ -> T.Subst.eval subst f
    | F.True
    | F.False -> f
    | F.Exists (var,f') ->
      (* replace [v] by a fresh skolem term *)
      let t = Skolem.skolem_form ~ctx subst var f in
      Util.debugf 2 ~section "@[<2>bind `%a` to@ `@[%a@]`@ :subst {%a}@]"
        (fun k->k Var.pp_fullc var T.pp t T.Subst.pp subst);
      let subst = Var.Subst.add subst var t in
      skolemize subst f'
    | F.Forall (var,f') ->
      let var' = Var.copy var in
      Util.debugf 5 ~section "@[<2>rename `%a` to@ `%a`@ :subst {%a}@]"
        (fun k->k Var.pp_fullc var Var.pp_fullc var' T.Subst.pp subst);
      let subst = Var.Subst.add subst var (T.var var') in
      skolemize subst f'
  in
  let res = skolemize f in
  Util.exit_prof prof_skolemize;
  res

(* For the following, we use "handbook of automated reasoning",
   chapter "compute small clause normal forms". We use a naive computation
   of clause sizes, but with a caching mechanism to block the exponential
   re-computation of sizes.
   The criterion for renaming is: if renaming makes less clauses, then
   always do it *)

(* estimation for a number of clauses *)
module Estimation = struct
  type t =
    | Exactly of int
    | TooBig

  let limit = max_int lsr 2

  (* lift "regular" operations on natural numbers *)
  let lift2 op a b = match a, b with
    | TooBig, _
    | _, TooBig -> TooBig
    | Exactly a, Exactly b ->
      let c = op a b in
      if (a>0 && b>0 && (c < a || c < b)) (* overflow *)
      || c > limit then TooBig else Exactly c

  let ( +/ ) = lift2 (+)
  let ( */ ) = lift2 ( * )

  (* comparison, but also assume that if both are too big, the first is bigger *)
  let geq_or_big e1 e2 = match e1, e2 with
    | TooBig, _ -> true
    | Exactly e', Exactly e'' -> e' >= e''
    | Exactly _, TooBig -> false

  let pp out = function
    | TooBig -> Fmt.string out "<too big>"
    | Exactly n -> Fmt.int out n
end

(* estimate the number of clauses needed by this formula. *)
let estimate_num_clauses ~pos f =
  Util.enter_prof prof_estimate;
  let module E = Estimation in
  (* recursive function.
     @param pos true if the formula is positive, false if it's negated *)
  let rec num pos f = match F.view f, pos with
    | F.Eq _, _
    | F.Neq _, _
    | F.Atom _, _
    | F.True, _
    | F.False, _ -> E.Exactly 1
    | F.Not f', _ -> num (not pos) f'
    | F.And l, true -> sum_list pos l
    | F.And l, false -> prod_list pos l
    | F.Or l, true -> prod_list pos l
    | F.Or l, false -> sum_list pos l
    | F.Imply (a,b), true -> E.(num false a */ num true b)
    | F.Imply (a,b), false -> E.(num true a +/ num false b)
    | F.Equiv(a,b), true ->
      E.((num true a */ num false b) +/ (num false a */ num true b))
    | F.Equiv(a,b), false ->
      E.((num true a */ num true b) +/ (num false a */ num false b))
    | F.Xor(a,b), true ->
      (* a xor b is defined as  (not a) <=> b *)
      E.((num false a */ num false b) +/ (num true a */ num true b))
    | F.Xor(a,b), false ->
      E.((num false a */ num true b) +/ (num true a */ num false b))
    | F.Forall (_, f'), _
    | F.Exists (_, f'), _ -> num pos f'
  and sum_list pos l = match l with
    | [] -> E.Exactly 0
    | x :: tail -> E.(num pos x +/ sum_list pos tail)
  and prod_list pos l = match l with
    | [] -> E.Exactly 1
    | x :: tail -> E.(num pos x */ prod_list pos tail)
  in
  let n = num pos f in
  Util.exit_prof prof_estimate;
  n

(* atomic formula, or forall/exists/not an atomic formula (1 literal) *)
let rec will_yield_lit f = match F.view f with
  | F.Not f'
  | F.Exists (_, f')
  | F.Forall (_, f') -> will_yield_lit f'
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False -> true
  | F.And _
  | F.Or _
  | F.Imply _
  | F.Equiv _
  | F.Xor _ -> false

(* introduce definitions for sub-formulas of [f], is needed. This might
   modify [ctx] by adding definitions to it, and it will {!NOT} introduce
   definitions in the definitions (that has to be done later).
   @param is_pos if true, polarity=pos, else polarity=false *)
let introduce_defs ~ctx ~is_pos stmt f =
  let module E = Estimation in
  (* shortcut to compute the number of clauses *)
  let p pos f = estimate_num_clauses ~pos f in
  let _neg = function
    | `Pos -> `Neg
    | `Neg -> `Pos
    | `Both -> `Both
  (* rename formula *)
  and rename_form ~polarity f =
    let def =
      Skolem.define_form ~ctx ~rw_rules:false ~polarity
        ~parents:[Stmt.as_proof_i stmt |> Proof.Parent.from]
        f
    in
    let p = def.Skolem.proxy in
    Util.debugf ~section 4
      "@[<2>introduce@ def. @[%a@]@ for subformula `@[%a@]`@ with pol %a@]"
      (fun k->k T.pp p T.pp f Skolem.pp_polarity polarity);
    p
  in
  (* recurse in sub-formulas, renaming as needed.
     a is the multiplicative factor for the number of clauses of (cnf f)
     b is the multiplicative factor for the number of clauses of (cnf ~f)
     polarity is the polarity of f within the outermost formula *)
  let rec maybe_rename ~polarity a b f =
    let f = maybe_rename_subformulas ~polarity a b f in
    (* depending on polarity and subformulas, do renamings.
       The condition is (where p is the expected number of clauses):
       if pol=1,  a * p(F) >= a + p(F)
       if pol=-1, b * p(~F) >= b + p(~F)
       if pol=0, a * p(F) + b * p(~F) >= a + b + p(F) + p(~F)
    *)
    let c1, c2 = match polarity with
      | `Pos -> E.(a */ p true f, a +/ p true f)
      | `Neg -> E.(b */ p false f, b +/ p false f)
      | `Both ->
        E.( a */ p true f +/ b */ p false f
          , a +/ b +/ p true f +/ p false f)
    in
    let should_rename = E.geq_or_big c1 c2 in
    if not (will_yield_lit f) && should_rename then (
      let f' = rename_form ~polarity f in
      Util.debugf ~section 5 "rename because pol=%a, a=%a, b=%a, c1=%a, c2=%a"
        (fun k->k Skolem.pp_polarity polarity E.pp a E.pp b E.pp c1 E.pp c2);
      f'
    ) else f
  (* introduce definitions for subterms *)
  and maybe_rename_subformulas ~polarity a b f = match F.view f with
    | F.True
    | F.False
    | F.Atom _
    | F.Eq _
    | F.Neq _ -> f
    | F.And l ->
      let l' =
        List.mapi
          (fun i f' ->
             let a' = a in
             let b' = E.(b */ prod_p ~pos:false ~except:i l 0) in
             maybe_rename ~polarity a' b' f')
          l
      in
      F.and_ l'
    | F.Or l ->
      let l' =
        List.mapi
          (fun i f' ->
             let a' = E.(a */ prod_p ~pos:true ~except:i l 0) in
             let b' = b in
             maybe_rename ~polarity a' b' f')
          l
      in
      F.or_ l'
    | F.Not f' ->
      let a' = b and b' = a in
      F.not_ (maybe_rename ~polarity:(_neg polarity) a' b' f')
    | F.Imply (f1, f2) ->
      let f1' =
        let a' = b and b' = E.(a */ p true f2) in
        maybe_rename ~polarity:(_neg polarity) a' b' f1
      and f2' =
        let a' = E.(a */ p false f1) and b' = b in
        maybe_rename ~polarity a' b' f2
      in
      F.imply f1' f2'
    | F.Equiv (f1, f2) ->
      let f1' =
        let a' = E.(a */ p false f2 +/ b */ p true f2) in
        let b' = E.(a */ p true f2 +/ b */ p false f2) in
        maybe_rename ~polarity:`Both a' b' f1
      and f2' =
        let a' = E.(a */ p false f1 +/ b */ p true f1) in
        let b' = E.(a */ p true f1 +/ b */ p false f1) in
        maybe_rename ~polarity:`Both a' b' f2
      in
      F.equiv f1' f2'
    | F.Xor (f1, f2) ->
      (* we consider that f1 has reverted polarity *)
      let f1' =
        let b' = E.(a */ p false f2 +/ b */ p true f2) in
        let a' = E.(a */ p true f2 +/ b */ p false f2) in
        maybe_rename ~polarity:`Both a' b' f1
      and f2' =
        let a' = E.(a */ p true f1 +/ b */ p false f1) in
        let b' = E.(a */ p false f1 +/ b */ p true f1) in
        maybe_rename ~polarity:`Both a' b' f2
      in
      F.xor f1' f2'
    | F.Forall (var, f') ->
      F.forall var (maybe_rename ~polarity a b f')
    | F.Exists (var, f') ->
      F.exists var (maybe_rename ~polarity a b f')
  (* product of all (p ~pos x) for x in l if idx(x) != except *)
  and prod_p ~pos l i ~except = match l with
    | [] -> E.Exactly 1
    | x :: tail ->
      if i = except
      then prod_p ~pos tail (i+1) ~except
      else
        let p_x = estimate_num_clauses ~pos x in
        E.(p_x */ prod_p ~pos tail (i+1) ~except)
  in
  let polarity = if is_pos then `Pos else `Neg in
  maybe_rename ~polarity (E.Exactly 1) (E.Exactly 0) f

(* helper: reduction to cnf using De Morgan laws. Returns a list of list of
   atomic formulas *)
let rec to_cnf_rec f = match F.view f with
  | F.Eq _
  | F.Neq _
  | F.Atom _
  | F.True
  | F.False
  | F.Not _ -> [[as_lit f]]
  | F.And l ->
    (* simply concat sub-CNF *)
    CCList.flat_map to_cnf_rec l
  | F.Or [] -> assert false
  | F.Or l ->
    Util.map_product ~f:to_cnf_rec l
  | F.Forall _
  | F.Exists _ ->
    errorf_
      "@[<2>Cnf.to_cnf_rec:@ can only clausify a skolemized formula,@ not @[%a@]@]"
      T.pp f
  | F.Xor _
  | F.Imply _
  | F.Equiv _ ->
    errorf_
      "@[<2>Cnf.to_cnf_rec:@ can only clausify a NNF formula,@ not @[%a@]@]"
      T.pp f

let to_cnf f =
  Util.enter_prof prof_to_cnf;
  let res = to_cnf_rec f in
  Util.exit_prof prof_to_cnf;
  res

type options =
  | DistributeExists
  | DisableRenaming
  | InitialProcessing of (form -> form) (** any processing, at the beginning *)
  | PostNNF of (form -> form)  (** any processing that keeps negation at leaves *)
  | PostSkolem of (form -> form) (** must not introduce variables nor negations *)

(* return new sources, without modifying anything *)
let new_src ~ctx : Stmt.input_t list =
  Skolem.new_definitions ~ctx
  |> CCList.flat_map Skolem.def_as_stmt

let rule_flatten = Proof.Rule.mk "cnf.flatten"
let rule_rename = Proof.Rule.mk "cnf.tseitin"

(* proof for a preprocessing step *)
let proof_preprocess stmt defs rule : Proof.Step.t =
  let stmt = Stmt.as_proof_i stmt in
  let defs = List.map Stmt.as_proof_i defs in
  Proof.Step.esa ~rule (List.map Proof.Parent.from (stmt::defs))

(* pop and return new statements *)
let pop_new_defs ~ctx : (_,_,_) Stmt.t list =
  Skolem.pop_new_definitions ~ctx
  |> CCList.flat_map Skolem.def_as_stmt

let pp_stmt out st = Stmt.pp T.pp T.pp_inner T.pp_inner out st

(* flatten definitions, removing some constructs such as if/match,
   introducing new definitions *)
let flatten ~ctx seq : _ Sequence.t =
  let open Flatten in
  let flatten_axiom stmt f =
    begin
      let vars, body = F.unfold_forall f in
      flatten_rec ctx stmt Pos_toplevel vars body >>= fun body ->
      get_subst >|= fun subst ->
      let vars = List.map (Var.update_ty ~f:(T.Subst.eval subst)) vars in
      let body = T.Subst.eval subst body in
      F.forall_l vars body
    end |> to_list'
  in
  let flatten_def stmt d : _ Stmt.def_rule list = match d with
    | Stmt.Def_term {vars;id;ty;args;rhs;as_form} ->
      begin
        flatten_rec_l ~of_:id ctx stmt Pos_inner vars args >>= fun args ->
        flatten_rec_l ~of_:id ctx stmt Pos_toplevel vars args >>= fun args ->
        flatten_rec ~of_:id ctx stmt Pos_toplevel vars rhs >>= fun rhs ->
        get_subst >|= fun subst ->
        let args = List.map (T.Subst.eval subst) args in
        let rhs = T.Subst.eval subst rhs in
        args, rhs
      end
      |> to_list'
      |> List.map (fun (args,rhs) -> Stmt.Def_term {vars;id;ty;args;rhs;as_form})
    | Stmt.Def_form {vars;lhs;rhs;polarity;as_form} ->
      begin
        let of_ = match lhs with
          | SLiteral.Atom (t,_) -> T.head t
          | _ -> None
        in
        map_sliteral (flatten_rec ?of_ ctx stmt Pos_inner vars) lhs >>= fun lhs ->
        flatten_rec_l ?of_ ctx stmt Pos_toplevel vars rhs >>= fun rhs ->
        get_subst >|= fun subst ->
        let lhs = SLiteral.map ~f:(T.Subst.eval subst) lhs in
        let rhs = List.map (T.Subst.eval subst) rhs in
        lhs, rhs
      end
      |> to_list'
      |> List.map
        (fun (lhs,rhs) -> Stmt.Def_form {vars;lhs;rhs;polarity;as_form})
  in
  seq
  |> Sequence.flat_map_l
    (fun stmt ->
       let n = Skolem.counter ctx in
       let proof() =
         if Skolem.counter ctx > n
         then proof_preprocess stmt (new_src ~ctx) rule_flatten
         else Stmt.proof_step stmt
       in
       let attrs = stmt.Stmt.attrs in
       let new_sts = match stmt.Stmt.view with
         | Stmt.Data _
         | Stmt.TyDecl _ -> [stmt]
         | Stmt.Rewrite d ->
           flatten_def stmt d
           |> List.map
             (fun d' ->
                Stmt.rewrite ~attrs ~proof:(proof ()) d')
         | Stmt.Def l ->
           let l =
             List.map
               (fun d ->
                  let rules = CCList.flat_map (flatten_def stmt) d.Stmt.def_rules in
                  { d with Stmt.def_rules=rules })
               l
           in
           [Stmt.def ~attrs ~proof:(proof ()) l]
         | Stmt.Assert f ->
           flatten_axiom stmt f
           |> List.map (fun f -> Stmt.assert_ ~attrs ~proof:(proof()) f)
         | Stmt.Lemma l ->
           List.map
             (fun f -> Stmt.lemma ~attrs ~proof:(proof ()) [F.and_ (flatten_axiom stmt f)]) l
         | Stmt.Goal f ->
           [Stmt.goal ~attrs ~proof:(proof ()) (F.and_ (flatten_axiom stmt f))]
         | Stmt.NegatedGoal _ -> assert false
       in
       Util.debugf ~section 5 "@[<2>flatten `@[%a@]`@ into `@[%a@]`@]"
         (fun k->k pp_stmt stmt (Util.pp_list pp_stmt) new_sts);
       begin match pop_new_defs ~ctx with
         | [] -> new_sts
         | l -> List.rev (List.rev_append new_sts l)
       end)

let is_rw stmt = match Stmt.view stmt with
  | Stmt.Rewrite _
  | Stmt.Def _ -> true
  | _ -> false

(* simplify formulas and rename them. May introduce new formulas *)
let simplify_and_rename ~ctx ~disable_renaming ~preprocess seq =
  Util.enter_prof prof_simplify_rename;
  (* process a formula *)
  let process_form ~is_goal stmt f =
    Util.debugf ~section 4 "@[<2>simplify and rename@ `@[%a@]`@]" (fun k->k T.pp f);
    (* preprocessing *)
    let f = List.fold_left (|>) f preprocess in
    (* introduce definitions? *)
    if disable_renaming || is_clause f || is_rw stmt
    then f
    else introduce_defs ~is_pos:(not is_goal) ~ctx stmt f
  in
  let process_def stmt d = match d with
    | Stmt.Def_term {vars;id;ty=ty_id;args;rhs;as_form} ->
      (* turn [p = rhs], partially applied (type […→…])
         into the def [p vars=rhs vars] *)
      let ty_vars, ty_args, ty_ret = T.Ty.unfold (T.ty_exn rhs) in
      if ty_vars<>[] then (
        errorf_
          "cannot deal with polymorphic definition@ `%a`"
          (Stmt.pp_def_rule T.pp T.pp T.pp) d;
      );
      if ty_args=[] then Stmt.Def_term{vars;id;ty=ty_id;args;rhs;as_form}
      else (
        let new_vars =
          List.mapi (fun i ty -> Var.makef ~ty "x_%d" i) ty_args
        in
        let vars = vars @ new_vars in
        let rhs =
          T.app_whnf ~ty:ty_ret rhs (List.map T.var new_vars)
        in
        if T.Ty.is_prop ty_ret then (
          let lhs_t =
            T.app_whnf ~ty:ty_ret (T.const ~ty:ty_id id)
              (args @ List.map T.var new_vars)
          and rhs_t =
            process_form stmt ~is_goal:false rhs
          in
          let as_form = [T.Form.equiv lhs_t rhs_t ] in
          let lhs = SLiteral.atom_true lhs_t in
          let rhs = [ rhs_t ] in
          Stmt.Def_form {vars;lhs;rhs;polarity=`Equiv;as_form}
        ) else (
          let vars = vars @ new_vars in
          let args = args @ List.map T.var new_vars in
          Stmt.Def_term {vars;id;ty=ty_id;args;rhs;as_form}
        )
      )
    | Stmt.Def_form {vars;polarity;lhs;rhs;as_form} ->
      let rhs = List.map (process_form stmt ~is_goal:false) rhs in
      Stmt.Def_form {lhs;rhs;vars;polarity;as_form}
  in
  let res =
    seq
    |> Sequence.flat_map
      (fun stmt ->
         let old_counter = Skolem.counter ctx in
         let proof() =
           if Skolem.counter ctx > old_counter
           then proof_preprocess stmt (new_src ~ctx) rule_rename
           else Stmt.proof_step stmt
         in
         let attrs = Stmt.attrs stmt in
         let new_st = match stmt.Stmt.view with
           | Stmt.Data _
           | Stmt.TyDecl _ -> stmt
           | Stmt.Def l ->
             let l =
               List.map
                 (fun d ->
                    let rules =
                      List.map (process_def stmt) d.Stmt.def_rules
                    in
                    let new_d = { d with Stmt.def_rules=rules } in
                    Util.debugf ~section 5
                      "(@[simplify-def@ `@[%a@]`@ :into `@[%a@]`@])"
                      (fun k->
                         let pp_st = Stmt.pp_def T.pp T.pp T.pp in
                         k pp_st d pp_st new_d);
                    new_d)
                 l
             in
             Stmt.def ~attrs ~proof:(proof ()) l
           | Stmt.Rewrite d ->
             let d = process_def stmt d in
             Stmt.rewrite ~attrs ~proof:(proof ()) d
           | Stmt.Assert f ->
             let f = process_form stmt ~is_goal:false f in
             Stmt.assert_ ~attrs ~proof:(proof ()) f
           | Stmt.Lemma l ->
             let l = List.map (process_form stmt ~is_goal:true) l in
             Stmt.lemma ~attrs ~proof:(proof()) l
           | Stmt.Goal f ->
             let f = process_form stmt ~is_goal:true f in
             Stmt.goal ~attrs ~proof:(proof()) f
           | Stmt.NegatedGoal _ -> assert false
         in
         begin match pop_new_defs ~ctx with
           | [] -> Sequence.return new_st
           | l -> Sequence.of_list (List.rev (new_st :: l))
         end
      )
    |> CCVector.of_seq ?init:None
  in
  Util.exit_prof prof_simplify_rename;
  res

type f_statement = (term, term, type_) Statement.t
(** A statement before CNF *)

type c_statement = (clause, term, type_) Statement.t
(** A statement after CNF *)

let id_ x = x

let rule_cnf = Proof.Rule.mk "cnf"
let rule_neg = Proof.Rule.mk "cnf.neg"

let proof_cnf stmt =
  Proof.Step.esa ~rule:rule_cnf
    [Stmt.as_proof_i stmt |> Proof.Parent.from]

let proof_neg stmt =
  Proof.Step.esa ~rule:rule_neg
    [Stmt.as_proof_i stmt |> Proof.Parent.from]

(* Transform the clauses into proper CNF; returns a list of clauses *)
let cnf_of_seq ?(opts=[]) ?(ctx=Skolem.create ()) seq =
  (* read options *)
  let disable_renaming = List.mem DisableRenaming opts in
  let preprocess =
    CCList.filter_map
      (function InitialProcessing f -> Some f | _ -> None)
      opts
  and post_nnf =
    CCList.filter_map
      (function PostNNF f -> Some f | _ -> None)
      opts
  and post_skolem =
    CCList.filter_map
      (function PostSkolem f -> Some f | _ -> None)
      opts
  in
  (* simplify and introduce definitions *)
  let v =
    flatten ~ctx seq
    |> simplify_and_rename ~ctx ~disable_renaming ~preprocess
  in
  (* reduce the new formulas to CNF *)
  let res = CCVector.create () in
  (* convert formula into CNF, returning a list of clauses and a list of skolems *)
  let conv_form_sk f : (ID.t * type_) list * clause list =
    Util.debugf ~section 4 "@[<2>reduce@ `@[%a@]`@ to CNF@]" (fun k->k T.pp f);
    let clauses =
      try as_cnf f
      with NotCNF _ | SLiteral.NotALit _ ->
        let f = nnf f in
        (* processing post-nnf *)
        let f = List.fold_left (|>) f post_nnf in
        Util.debugf ~section 4 "@[<2>... NNF:@ `@[%a@]`@]" (fun k->k T.pp f);
        let distribute_exists = List.mem DistributeExists opts in
        let f = miniscope ~distribute_exists f in
        Util.debugf ~section 4 "@[<2>... miniscoped:@ `@[%a@]`@]" (fun k->k T.pp f);
        (* adjust the variable counter to [f] before skolemizing *)
        let f = skolemize ~ctx Var.Subst.empty f in
        (* processing post-skolemization *)
        let f = List.fold_left (|>) f post_skolem in
        Util.debugf ~section 4 "@[<2>... skolemized:@ `@[%a@]`@]" (fun k->k T.pp f);
        let clauses = to_cnf f in
        Util.debugf ~section 4 "@[<2>... CNF:@ `@[%a@]`@]"
          (fun k->k (Util.pp_list ~sep:", " pp_clause) clauses);
        clauses
    in
    let new_ids = Skolem.pop_new_skolem_symbols ~ctx in
    List.iter
      (fun (id,ty) ->
         let proof = Proof.Step.define_internal id [] in
         CCVector.push res (Stmt.ty_decl ~proof id ty))
      new_ids;
    new_ids, clauses
  in
  let conv_form f =
    snd (conv_form_sk f)
  in
  let conv_def d : _ Stmt.def_rule list = match d with
    | Stmt.Def_term {vars;id;ty;args;rhs;_} ->
      let as_form : clause =
        [SLiteral.eq (T.app (T.const ~ty id) ~ty:(T.ty_exn rhs) args) rhs]
      in
      [ Stmt.Def_term {vars;id;ty;args;rhs;as_form} ]
    | Stmt.Def_form {vars;lhs;rhs;polarity;_} ->
      let conv_polarized_default() =
        (* polarized rewriting: make two rewrite rules, one positive,
           one negative.
           positive:   lhs <=> cnf(rhs)
           negative: ¬ lhs <=> cnf(¬ rhs) *)
        let c_pos () =
          let rhs = CCList.flat_map conv_form rhs in
          let as_form = List.map (fun rhs -> SLiteral.negate lhs :: rhs) rhs in
          Stmt.Def_form {vars;lhs;rhs;polarity=`Imply;as_form}
        and c_neg () =
          let rhs = conv_form (F.not_ (F.and_ rhs)) in
          let as_form : clause list =
            List.map (fun rhs -> lhs :: List.map SLiteral.negate rhs) rhs
          in
          Stmt.Def_form {
            vars;lhs=SLiteral.negate lhs; rhs; as_form; polarity=`Imply;
          }
        in
        begin match polarity with
          | `Imply -> [ c_pos() ]
          | `Equiv -> [ c_pos(); c_neg() ]
        end
      in
      (* see if the rule can be cast as a (boolean) term rule *)
      begin match lhs, rhs with
        | SLiteral.Atom(lhs_t,true), [rhs_t] ->
          begin match T.as_id_app lhs_t, as_lit_opt rhs_t with
            | Some (id, ty_id, args), Some lit when SLiteral.is_pos lit ->
              (* convert to term rule *)
              let rhs = SLiteral.to_form lit in
              let as_form : clause =
                [SLiteral.eq
                   (T.app (T.const ~ty:ty_id id) ~ty:(T.ty_exn rhs) args) rhs]
              in
              [ Stmt.Def_term {vars;id;ty=ty_id;args;rhs;as_form} ]
            | _ -> conv_polarized_default()
          end
        | _ -> conv_polarized_default()
      end
  in
  CCVector.iter
    (fun stmt ->
       let proof = Stmt.proof_step stmt in
       let attrs = Stmt.attrs stmt in
       match stmt.Stmt.view with
         | Stmt.Assert f ->
           let proof = proof_cnf stmt in
           List.iter
             (fun c -> CCVector.push res (Stmt.assert_ ~attrs ~proof c))
             (conv_form f)
         | Stmt.Def l ->
           let l =
             List.map
               (fun d ->
                  let rules =
                    d.Stmt.def_rules |> CCList.flat_map conv_def
                  in
                  { d with Stmt.def_rules = rules })
               l
           in
           CCVector.push res (Stmt.def ~attrs ~proof l);
         | Stmt.Rewrite d ->
           let st_l =
             conv_def d
             |> List.map (Stmt.rewrite ~attrs ~proof)
           in
           CCVector.append_list res st_l;
         | Stmt.Data l ->
           CCVector.push res (Stmt.data ~attrs ~proof l)
         | Stmt.TyDecl (id,ty) ->
           CCVector.push res (Stmt.ty_decl ~attrs ~proof id ty)
         | Stmt.Lemma l ->
           let proof = proof_cnf stmt in
           let l = CCList.flat_map conv_form l in
           CCVector.push res (Stmt.lemma ~attrs ~proof l)
         | Stmt.Goal f ->
           (* intermediate statement to represent the negation step *)
           let not_f = F.not_ f in
           let stmt = Stmt.neg_goal ~proof:(proof_neg stmt) ~skolems:[] [not_f] in
           (* now take the CNF of negated goal *)
           let skolems, l = conv_form_sk not_f in
           let proof = proof_cnf stmt in
           CCVector.push res (Stmt.neg_goal ~attrs ~proof ~skolems l)
         | Stmt.NegatedGoal (sk1,l) ->
           let proof = proof_cnf stmt in
           let skolems, l =
             CCList.fold_flat_map
               (fun sk f ->
                  let sk', clauses = conv_form_sk f in
                  List.rev_append sk' sk, clauses)
               sk1 l
           in
           CCVector.push res (Stmt.neg_goal ~attrs ~proof ~skolems l)
    )
    v;
  (* return final vector of clauses *)
  CCVector.freeze res

let cnf_of ?opts ?ctx st =
  cnf_of_seq ?opts ?ctx (Sequence.return st)

let pp_f_statement out st = Statement.pp T.pp T.pp T.pp out st

let pp_c_statement out st =
  let pp_clause out c =
    Format.fprintf out "@[<hv>%a@]" (Util.pp_list ~sep:" ∨ " (SLiteral.pp T.pp)) c
  in
  Statement.pp pp_clause T.pp T.pp out st

let pp_fo_c_statement = Statement.pp_clause

let type_declarations seq =
  let open Statement in
  seq
  |> Sequence.flat_map Seq.ty_decls
  |> ID.Map.of_seq

let convert seq =
  let module A = UntypedAST in
  (* used for conversion *)
  let t_ctx = Term.Conv.create() in
  let ty_ctx = Type.Conv.create() in
  let conv_t = Term.Conv.of_simple_term_exn t_ctx in
  let conv_ty = Type.Conv.of_simple_term_exn ty_ctx in
  let conv_def = function
    | Stmt.Def_term {vars;id;ty;args;rhs;as_form} ->
      let vars = List.map (Var.update_ty ~f:conv_ty) vars in
      let as_form = List.map (SLiteral.map ~f:conv_t) as_form in
      Stmt.Def_term{vars;id;ty=conv_ty ty;args=List.map conv_t args;
                    rhs=conv_t rhs;as_form}
    | Stmt.Def_form {vars;lhs;rhs;polarity;_} ->
      let vars = List.map (Var.update_ty ~f:conv_ty) vars in
      let lhs = SLiteral.map ~f:conv_t lhs in
      let rhs = List.map (clause_to_fo ~ctx:t_ctx) rhs in
      let as_form = List.map (fun rhs -> lhs :: rhs) rhs in
      Stmt.Def_form { vars;lhs;rhs;polarity;as_form}
  in
  let conv_statement st =
    Util.debugf ~section 5
      "@[<2>@{<yellow>convert@}@ `@[%a@]`@]" (fun k->k pp_c_statement st);
    let attrs = Stmt.attrs st in
    let proof = Stmt.proof_step st in
    let res = match Stmt.view st with
      | Stmt.Goal c ->
        let c = clause_to_fo ~ctx:t_ctx c in
        Stmt.goal ~attrs ~proof c
      | Stmt.NegatedGoal (sk,l) ->
        let skolems = List.map (fun (id,ty)->id, conv_ty ty) sk in
        let l = List.map (clause_to_fo ~ctx:t_ctx) l in
        Stmt.neg_goal ~attrs ~proof ~skolems l
      | Stmt.Lemma l ->
        let l = List.map (clause_to_fo ~ctx:t_ctx) l in
        Stmt.lemma ~attrs ~proof l
      | Stmt.Assert c ->
        let c = clause_to_fo ~ctx:t_ctx c in
        Stmt.assert_ ~attrs ~proof c
      | Stmt.Data l ->
        let l = List.map (Stmt.map_data ~ty:conv_ty) l in
        Stmt.data ~attrs ~proof l
      | Stmt.Def l ->
        let l =
          List.map
            (fun d ->
               Stmt.map_def d ~term:conv_t ~ty:conv_ty
                 ~form:(clause_to_fo ~ctx:t_ctx))
            l
        in
        Stmt.def ~attrs ~proof l
      | Stmt.Rewrite d ->
        Stmt.rewrite ~attrs ~proof (conv_def d)
      | Stmt.TyDecl (id, ty) ->
        let ty = conv_ty ty in
        Stmt.ty_decl ~attrs ~proof id ty
    in
    Util.debugf ~section 3
      "@[@[<2>convert@ `@[%a@]`@]@ @[<2>into `@[%a@]`@]@]"
      (fun k->k pp_c_statement st Stmt.pp_clause res);
    res
  in
  Sequence.map conv_statement seq
  |> CCVector.of_seq
  |> CCVector.freeze

