
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compute Precedence} *)

module T = Term

let prof_mk_prec = Util.mk_profiler "mk_precedence"

let section = Util.Section.(make ~parent:root) "compute_prec"

let _alpha_precedence = ref false
let _custom_weights = ref ""

type 'a parametrized = Statement.clause_t Sequence.t -> 'a

type t = {
  constrs : (int * [`partial] Precedence.Constr.t) list;
  constr_rules : (int * [`partial] Precedence.Constr.t parametrized) list;
  last_constr : [`total] Precedence.Constr.t;
  weight_rule : Precedence.weight_fun parametrized;
  status : (ID.t * Precedence.symbol_status) list;
}

(* uniform weight *)
let _default_weight _ = Precedence.weight_constant

let _default_arg_coeff _ = []

let empty =
  { constrs = [];
    constr_rules = [];
    last_constr = Precedence.Constr.alpha;
    weight_rule = _default_weight;
    status = [];
  }

let add_constr p c t = { t with constrs=(p,c)::t.constrs; }

let add_constrs l = List.fold_right (fun (p,c) -> add_constr p c) l

let add_constr_rule p r t = { t with constr_rules = (p, r) :: t.constr_rules }

let set_weight_rule r t = { t with weight_rule = r }

let add_status l t = { t with status = List.rev_append l t.status }

(* Add weights specified by the user using the cli option *)
let _add_custom_weights weights arg_coeff=
  let input_list = CCString.split_on_char ',' !_custom_weights in
  if input_list = [""] then weights, arg_coeff
  else List.fold_left (fun (weights, arg_coeff) input ->
      try
        let i = String.index input '=' in
        let name = String.sub input 0 i in
        let values =
          String.sub input (i+1) (String.length input - i - 1)
          |> CCString.split_on_char ':'
          |> List.map int_of_string
        in
        (fun constant ->
           if ID.name constant = name then Precedence.Weight.int (List.hd values)
           else weights constant),
        (fun constant ->
           if ID.name constant = name then List.tl values
           else arg_coeff constant)
      with
        | Failure _ | Not_found -> failwith "Syntax error in custom weights"
    ) (weights, arg_coeff) input_list

let mk_precedence t seq =
  Util.enter_prof prof_mk_prec;
  (* set of symbols *)
  let symbols =
    seq
    |> Sequence.flat_map Statement.Seq.symbols
    |> ID.Set.of_seq
    |> ID.Set.to_list
  in
  (* constraints *)
  let constrs =
    t.constrs @
      List.map (fun (p,rule) -> p, rule seq) t.constr_rules
  in
  Util.debugf ~section 2 "@[<2>%d precedence constraint(s)@]"
    (fun k->k(List.length constrs));
  let weight = t.weight_rule seq in
  let weight,arg_coeff = _add_custom_weights weight _default_arg_coeff in
  let constr = Precedence.Constr.compose_sort constrs in
  let constr = Precedence.Constr.compose constr t.last_constr in
  let constr = (if !_alpha_precedence then Precedence.Constr.alpha else constr) in
  let p = Precedence.create ~weight ~arg_coeff constr symbols in
  (* multiset status *)
  List.iter
    (fun (s,status) -> Precedence.declare_status p s status)
    t.status;
  Util.exit_prof prof_mk_prec;
  p

let () =
  Options.add_opts
    [  "--alpha-precedence"
    , Arg.Set _alpha_precedence
    , " use pure alphabetical precedence"
    ;  "--weights"
     , Arg.Set_string _custom_weights
     , " set weights, e.g. f=2,g=3,h=1, or weights and argument coefficients, e.g. f=2:3:4,g=3:2"
    ]
