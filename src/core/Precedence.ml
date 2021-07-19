
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Precedence (total ordering) on symbols} *)

module T = Term

type symbol_status =
  | Multiset
  | Lexicographic
  | LengthLexicographic

let section = Util.Section.(make "precedence")

let db_w_def = 1
let lmb_w_def = 1

(** {2 Weight of Symbols} *)
module Weight = struct
  type t = {
    omega: int;
    one: int;
  }
  (** [a, b] is [a·ω + b] *)

  let make omega one = {omega; one}

  let int i : t = make 0 i
  let zero = int  0
  let one = int 1
  let omega : t = make 1 0
  let omega_plus i : t = make 1 i

  let mult c a : t = {omega = a.omega * c; one = a.one * c}
  let mult_one c a : t = {a with one = a.one *c}

  let add a b: t = {omega=a.omega+b.omega; one=a.one+b.one}
  let diff a b: t = {omega=a.omega-b.omega; one=a.one-b.one}

  module Infix = struct
    let (+) = add
    let (-) = diff
  end
  include Infix

  let compare a b: int =
    if a.omega=b.omega
    then CCInt.compare a.one b.one
    else CCInt.compare a.omega b.omega

  let sign a: int = compare a zero

  let pp out (a:t): unit =
    if a.omega=0 then CCFormat.int out a.one
    else if a.one=0 then Format.fprintf out "%d@<1>·@<1>ω" a.omega
    else Format.fprintf out "%d@<1>·@<1>ω+%d" a.omega a.one
  let to_string = CCFormat.to_string pp
end

(** {2 Constraints} *)

let get_arity ~sig_ref s =
  try 
    snd (Signature.arity !sig_ref s)
  with Not_found -> 0

let is_not_fresh_sk = -1
let is_post_cnf_skolem ~sig_ref s =
  let ans = 
    ((not (Signature.is_empty !sig_ref)) && not (Signature.mem !sig_ref s))
    || (match ID.as_skolem s with 
      | Some ID.K_after_cnf -> true
      | _ -> false)
  in
  ans

let post_cnf_id ~sig_ref s =
  if is_post_cnf_skolem ~sig_ref s then ID.id s else is_not_fresh_sk

let on_signature_update = Signal.create()

let update_signature prev_sig signature =
  prev_sig := signature;
  Signal.ContinueListening

module Constr = struct
  type 'a t = ID.t -> ID.t -> int
    constraint 'a = [< `partial | `total]

  type prec_fun = signature:Signature.t -> ID.t Iter.t -> [`partial] t

  (* In the following functions, fresh symbols are made the smallest.
     However, newer fresh symbols are placed after older fresh symbols
     (using monotonicity of the value of ID.int)
   *)

  let arity ~signature _ s1 s2 =
    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);
    let open CCOrd in
    (* bigger arity means bigger symbol *)
    - (CCInt.compare (post_cnf_id ~sig_ref:_sig s1) (post_cnf_id ~sig_ref:_sig s2))
    <?> (CCInt.compare, get_arity ~sig_ref:_sig s1, get_arity ~sig_ref:_sig s2)
    <?> (ID.compare, s1, s2)

  let inv_arity ~signature _ s1 s2 =
    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);
    let open CCOrd in
    - (CCInt.compare (post_cnf_id ~sig_ref:_sig s1) (post_cnf_id ~sig_ref:_sig s2))
    <?> (CCInt.compare, get_arity ~sig_ref:_sig s2, get_arity ~sig_ref:_sig s1)
    <?> (ID.compare, s1, s2)

  let invfreqhack ~signature seq =
    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);

    (* symbol -> number of occurrences of symbol in seq *)
    let tbl = ID.Tbl.create 16 in
    Iter.iter (ID.Tbl.incr tbl) seq;
    let avg = 
      if Iter.length seq == 0 then 10
      else Iter.sum (ID.Tbl.values tbl) / Iter.length (ID.Tbl.values tbl) in
    let find_freq s = ID.Tbl.get_or ~default:avg tbl s in
    let max_unary_freq =
      Signature.Seq.symbols signature
      |> Iter.filter_map (fun id ->
        if snd @@ Signature.arity signature id == 1 
        then Some (find_freq id)
        else None 
      )
      |> Iter.max
      |> CCOpt.get_or ~default:max_int in
    (* compare by inverse frequency (higher frequency => smaller) *)
    let is_unary_max_freq _sig s1 =
      Signature.mem !_sig s1
      && get_arity ~sig_ref:_sig s1 == 1
      && find_freq s1 == max_unary_freq in
    
    let is_nullary _sig s1 =
      Signature.mem !_sig s1 
      && get_arity ~sig_ref:_sig s1 == 0 in

    fun s1 s2 ->
      let open CCOrd in
      (* criteria as in generate_invfreq_hack_precedence -- E source *)
      let categorize s =
        if is_post_cnf_skolem ~sig_ref:_sig s then (min_int, ID.id s, 0)
        else if is_nullary _sig s then (min_int+1, - (find_freq s), ID.id s)
        else if is_unary_max_freq _sig s then (max_int, 0, ID.id s)
        else (- (find_freq s), get_arity ~sig_ref:_sig s, ID.id s) in
      let (a1,a2,a3), (b1,b2,b3) = CCPair.map_same categorize (s1, s2) in
      CCInt.compare a1 b1
      <?> (CCInt.compare, a2, b2)
      <?> (CCInt.compare, a3, b3)

  let invfreq_constmin ~signature seq =
    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);

    (* symbol -> number of occurrences of symbol in seq *)
    let tbl = ID.Tbl.create 16 in
    Iter.iter (ID.Tbl.incr tbl) seq;
    let avg = 
      if Iter.length seq == 0 then 10
      else Iter.sum (ID.Tbl.values tbl) / Iter.length (ID.Tbl.values tbl) in
    let find_freq s = ID.Tbl.get_or ~default:avg tbl s in
    
    let is_nullary _sig s1 =
      Signature.mem !_sig s1 
      && get_arity ~sig_ref:_sig s1 == 0 in

    fun s1 s2 ->
      let open CCOrd in
      (* criteria as in generate_invfreq_hack_precedence -- E source *)
      let categorize s =
        if is_post_cnf_skolem ~sig_ref:_sig s then (min_int, ID.id s, 0)
        else if is_nullary _sig s then (min_int+1, - (find_freq s), ID.id s)
        else (- (find_freq s), get_arity ~sig_ref:_sig s, ID.id s) in
      let (a1,a2,a3), (b1,b2,b3) = CCPair.map_same categorize (s1, s2) in
      CCInt.compare a1 b1
      <?> (CCInt.compare, a2, b2)
      <?> (CCInt.compare, a3, b3)

  let invfreqconj ~signature seq =
    (* The set of conjecture symbols cannot increase, so we do not subscribe to
       signature changes*)
    let tbl = ID.Tbl.create 16 in
    Iter.iter (ID.Tbl.incr tbl) seq;
    let avg = 
      if Iter.length seq == 0 then 10
      else Iter.sum (ID.Tbl.values tbl) / Iter.length (ID.Tbl.values tbl) in
    let find_freq s = ID.Tbl.get_or ~default:avg tbl s in

    fun s1 s2 ->
      let open CCOrd in
      (* criteria as in generate_invfreq_hack_precedence -- E source *)
      let categorize s =
        if is_post_cnf_skolem ~sig_ref:(ref (signature)) s then (min_int, ID.id s, 0)
        else 
        ((if Signature.sym_in_conj s signature then 1 else 0),
          find_freq s, ID.id s) in
      let (a1,a2,a3), (b1,b2,b3) = CCPair.map_same categorize (s1, s2) in
      CCInt.compare a1 b1
      <?> (CCInt.compare, a2, b2)
      <?> (CCInt.compare, a3, b3)

  
  (* symbol -> number of occurrences of symbol in seq *)
  let invfreq ~signature seq =
    (* Does not use the signature *)
    let tbl = ID.Tbl.create 16 in
    Iter.iter (ID.Tbl.incr tbl) seq;
    let avg = 
      if Iter.length seq == 0 then 10
      else Iter.sum (ID.Tbl.values tbl) / Iter.length (ID.Tbl.values tbl) in
    let find_freq s = ID.Tbl.get_or ~default:avg tbl s in
    let sig_ref = ref signature in
    (* compare by inverse frequency (higher frequency => smaller) *)
    fun s1 s2 ->
      let open CCOrd in
      let n1 = find_freq s1 in
      let n2 = find_freq s2 in

      - (CCInt.compare (post_cnf_id ~sig_ref s1) (post_cnf_id ~sig_ref s2))
      (* post-cnf symbols have the same value of n2 and n1 *)
      <?> (CCInt.compare, n2, n1)
      <?> (ID.compare, s1, s2)
  
  let unary_first ~signature _ s1 s2 =
    let open CCOrd in
    
    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);
    
    let is_unary _sig s = get_arity ~sig_ref:_sig s == 1 in
    let weight _sig s =
      if is_post_cnf_skolem ~sig_ref:_sig s then min_int
      else if is_unary _sig s then max_int
      else if not (Signature.mem !_sig s) then max_int - 1
      else get_arity ~sig_ref:_sig s  in
    CCInt.compare (weight _sig s1) (weight _sig s2)
    <?> (ID.compare, s1, s2)

  let const_first ~signature _ s1 s2 =
    let open CCOrd in

    let _sig = ref signature in
    Signal.on on_signature_update (update_signature _sig);

    let is_const _sig s = get_arity ~sig_ref:_sig s == 0 in
    let weight _sig s =
      if is_post_cnf_skolem ~sig_ref:_sig s then min_int
      else if not (Signature.mem !_sig s) then max_int -1
      else if is_const _sig s then max_int
      else get_arity ~sig_ref:_sig s  in
    CCInt.compare (weight _sig s1) (weight _sig s2)
    <?> (ID.compare, s1, s2)

  let prec_fun_of_str name =
    let map = [
      ("arity", arity);
      ("invarity", inv_arity);
      ("invfreq", invfreq);
      ("invfreqhack", invfreqhack);
      ("invfreq_constmin", invfreq_constmin);
      ("invfreq_conj", invfreqconj);
      ("unary_first", unary_first);
      ("const_first", const_first);
    ] in
    match CCList.assoc_opt ~eq:CCString.equal name map with 
    | Some wfun -> wfun
    | None -> 
      let err =
        CCFormat.sprintf "precedences are one of: (@[%a@])" 
          (CCList.pp CCString.pp ~pp_sep:(CCFormat.return "|@,")) (List.map fst map) in
      invalid_arg err

  (* regular string ordering *)
  let alpha a b =
    let c = String.compare (ID.name a) (ID.name b) in
    if c = 0
    then ID.compare a b else c

  let max ~signature l =
    let set = ID.Set.of_iter l in
    fun s1 s2 ->
      let is_max1 = ID.Set.mem s1 set in
      let is_max2 = ID.Set.mem s2 set in
      match is_max1, is_max2 with
      | true, true
      | false, false -> 0
      | true, false -> 1
      | false, true -> -1

  let min ~signature l =
    let set = ID.Set.of_iter l in
    fun s1 s2 ->
      let is_min1 = ID.Set.mem s1 set in
      let is_min2 = ID.Set.mem s2 set in
      match is_min1, is_min2 with
      | true, true
      | false, false -> 0
      | true, false -> -1
      | false, true -> 1

  let compose a b s1 s2 =
    let c = a s1 s2 in
    if c=0 then b s1 s2 else c

  let compose_sort l =
    if l=[] then invalid_arg "Precedence.Constr.compose_sort";
    let l = List.sort (CCFun.compose_binop fst CCInt.compare) l in
    let rec mk = function
      | [] -> assert false
      | [_,o] -> o
      | (_,o1) :: tail ->
        let o2 = mk tail in
        compose o1 o2
    in
    mk l

  let compare_by ~constr a b = constr a b

  let make c = c
end

(* TODO: think about how to compare some builtins (true, false, numbers...) *)

type t = {
  mutable snapshot : ID.t list;
  (* symbols by increasing order *)
  mutable tbl: int ID.Tbl.t Lazy.t;
  (* symbol -> index in precedence *)
  status: symbol_status ID.Tbl.t;
  (* symbol -> status *)
  mutable weight: ID.t -> Weight.t;
  (* weight function *)
  mutable arg_coeff: ID.t -> int list;
  (* argument coefficients *)
  db_w : int;
  lmb_w : int;
  constr : [`total] Constr.t;
  (* constraint used to build and update the precedence *)
}

type precedence = t

let equal p1 p2 =
  try List.for_all2 ID.equal p1.snapshot p2.snapshot
  with Invalid_argument _ -> false

let snapshot p = p.snapshot

(* Precedence weight for E-like selection functions *)
let sel_prec_weight p s1 =
  let lazy tbl = p.tbl in
  ID.Tbl.get_or ~default:100 tbl s1

let compare_by_tbl p s1 s2 =
  let lazy tbl = p.tbl in
  let i1 = ID.Tbl.get_or ~default:~-1 tbl s1 in
  let i2 = ID.Tbl.get_or ~default:~-1 tbl s2 in
  let c = CCInt.compare i1 i2 in
  if c = 0
  then (
    assert (i1=(-1) && i2=(-1) || ID.equal s1 s2);
    c
  )
  else c

let compare p s1 s2 = match ID.as_parameter s1, ID.as_parameter s2 with
  | None, None -> compare_by_tbl p s1 s2
  | Some _, None -> -1
  | None, Some _ -> 1
  | Some i, Some j -> CCOrd.int i j

let mem p s =
  let lazy tbl = p.tbl in
  ID.Tbl.mem tbl s

let status p s = ID.Tbl.get_or ~default:LengthLexicographic p.status s

let weight p s = p.weight s

let db_weight p = Weight.int p.db_w
let lam_weight p = Weight.int p.lmb_w

let arg_coeff p s i = try List.nth (p.arg_coeff s) i with _ -> 1

let declare_status p s status =
  ID.Tbl.replace p.status s status

module Seq = struct
  let symbols p = Iter.of_list p.snapshot
end

let pp_ pp_id out l =
  Format.fprintf out "[@[<2>%a@]]" (Util.pp_list ~sep:" < " pp_id) l

let pp_snapshot out l = pp_ ID.pp out l

let pp out prec =
  let pp_id out s = match status prec s with
    | Multiset -> Format.fprintf out "%a[M]" ID.pp s
    | Lexicographic -> Format.fprintf out "%a[L]" ID.pp s
    | LengthLexicographic -> Format.fprintf out "%a" ID.pp s
  in
  pp_ pp_id out prec.snapshot

let pp_debugf out prec =
  let pp_id out s = match status prec s with
    | Multiset -> Format.fprintf out "%a[M]" ID.pp_full s
    | Lexicographic -> Format.fprintf out "%a[L]" ID.pp_full s
    | LengthLexicographic -> Format.fprintf out "%a" ID.pp_full s
  in
  pp_ pp_id out prec.snapshot

let to_string = CCFormat.to_string pp

(* build a table  symbol -> i. such as if
    [tbl s = i], then w[List.nth i l = s] *)
let mk_tbl_ l =
  let tbl = ID.Tbl.create 64 in
  List.iteri
    (fun i s -> ID.Tbl.add tbl s i)
    l;
  tbl

(** {3 Weight} *)

type weight_fun = ID.t -> Weight.t
type arg_coeff_fun = ID.t -> int list

(* constant weight *)
let default_weight = Weight.int 1
let weight_constant _ = default_weight
let empty_sig = Signature.empty

let depth_occ_driver ~flip stmt_d =
  let tbl = ID.Tbl.create 16 in
  Iter.iter (fun (sym,d) -> 
      try 
        let l = ID.Tbl.find tbl sym in
        ID.Tbl.replace tbl sym (d :: l)
      with _ -> ID.Tbl.add tbl sym [d]
    ) stmt_d;

  let rec sum = function 
    | [] -> 0
    | x :: xs -> x + (sum xs) in

  let sorted = 
    ID.Tbl.to_list tbl 
    |> List.map (fun (id, depths) -> 
        let d_sum = sum depths and n = List.length depths in
        (d_sum / n, n, id))
    |> List.sort (fun (avg1, n1, id1) (avg2, n2, id2) -> 
        let open CCOrd in
        if flip then compare avg2 avg1 <?> (compare, n2, n1) <?> (ID.compare, id2, id1)
        else compare avg1 avg2 <?> (compare, n1, n2) <?> (ID.compare, id1, id2)
      ) in

  ID.Tbl.clear tbl;
  let tbl = ID.Tbl.create 16 in
  List.iteri (fun i (_,_,sym) -> ID.Tbl.add tbl sym (Weight.int (i + 5))) sorted;
  let default = Weight.int 5 in
  fun sym -> (
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) sym then default_weight
    else ID.Tbl.get_or ~default tbl sym)

let inv_depth_occurrence = depth_occ_driver ~flip:false
let depth_occurrence = depth_occ_driver ~flip:true


let max_arity signature = 
  Signature.Seq.symbols signature
  |> Iter.map (fun sym -> snd @@ Signature.arity signature sym)
  |> Iter.max |> CCOpt.get_or ~default:max_int

(* weight of f = arity of f + 4 *)
let weight_modarity ~signature =
  let _sig = ref signature in
  Signal.on on_signature_update (update_signature _sig);

  fun a ->
    let arity =  try snd @@ Signature.arity !_sig a with _ -> 5 in
    if is_post_cnf_skolem ~sig_ref:_sig a then default_weight
    else Weight.int (arity + 4)

let weight_arity0 ~signature =
  let _sig = ref signature in
  Signal.on on_signature_update (update_signature _sig);

  let max_arity a b =
    match a, b with
    | None, x -> Some x
    | Some (_, arity1), (_,arity2) ->
      if arity2 > arity1 then Some b else a in

  let max_sym = 
    Signature.Seq.symbols signature
    |> Iter.fold (fun acc sym -> 
        max_arity acc (sym,ID.id sym)
      ) None
    |> CCOpt.map fst in
  
  function a ->
    let res = 
      match max_sym with 
      | None -> get_arity ~sig_ref:_sig a + 1
      (* no access to the precedence, cannot compute max symbol -- cannot assign 0 *)
      | Some m_id -> if ID.equal m_id a then 1 else (get_arity ~sig_ref:_sig a + 1)
    in
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) a then default_weight
    else Weight.int res
    

let weight_invarity ~signature =
  (* Depends on max arity, and cannot easily evolve during the proving *)
  let max_a = max_arity signature in
  (fun a ->
     let arity =  try snd @@ Signature.arity signature a with _ -> 0 in
     if is_post_cnf_skolem ~sig_ref:(ref empty_sig) a then default_weight
     else Weight.int (max_a - arity + 3))

let weight_sq_arity ~signature =
  let _sig = ref signature in
  Signal.on on_signature_update (update_signature _sig);
  fun a ->
    let arity =  try snd @@ Signature.arity !_sig a with _ -> 2 in
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) a then default_weight
    else Weight.int (arity * arity + 1)

let weight_invsq_arity ~signature =
  (* Depends on max arity, and cannot easily evolve during the proving *)
  let max_a = max_arity signature in
  (fun a -> 
    let arity =  try snd @@ Signature.arity signature a with _ -> max_a / 2 in
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) a then default_weight
    else Weight.int (max_a*max_a - arity * arity + 1))

let weight_invfreq (symbs : ID.t Iter.t) : ID.t -> Weight.t =
  let tbl = ID.Tbl.create 16 in
  Iter.iter (ID.Tbl.incr tbl) symbs;
  let max_freq = List.fold_left (max) 0 (ID.Tbl.values_list tbl) in
  fun sym ->
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) sym then default_weight
    else Weight.int (max_freq - (ID.Tbl.get_or ~default:(max_freq/2) tbl sym) + 5) 

let weight_freq (symbs : ID.t Iter.t) : ID.t -> Weight.t =
  let tbl = ID.Tbl.create 16 in
  Iter.iter (ID.Tbl.incr tbl) symbs;
  fun sym -> 
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) sym then default_weight
    else Weight.int ((ID.Tbl.get_or ~default:10 tbl sym) + 5) 

let weight_rank ~flip (symbs : ID.t Iter.t) : ID.t -> Weight.t =
  let tbl = ID.Tbl.create 16 in
  Iter.iter (ID.Tbl.incr tbl) symbs;
  let sorted = 
    CCList.sort (fun s1 s2 ->
        let w_s1 = ID.Tbl.get_or tbl ~default:0 s1 in
        let w_s2 = ID.Tbl.get_or tbl ~default:0 s2 in
        if flip then CCInt.compare w_s2 w_s1 else CCInt.compare w_s1 w_s2) (ID.Tbl.keys_list tbl) in
  let prev_step = ref (-1) in
  let w = ref 0 in
  List.iter (fun (sym:ID.t) -> 
    let num_occs = ID.Tbl.get_or tbl sym ~default:0 in
    if num_occs != !prev_step then (
      prev_step := num_occs;
      incr w
    );
    ID.Tbl.replace tbl sym !w) sorted;

  (fun sym -> 
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) sym then default_weight
    else Weight.int (ID.Tbl.get_or ~default:10 tbl sym))

let weight_invfreqrank = weight_rank ~flip:true
let weight_freqrank = weight_rank ~flip:false

(* This function takes base KBO weight function and adjusts it so
   that defined symbols are larger than its defitnitions. *)
let lambda_def_weight lm_w db_w base_weight clauses =
  let definition_map = ID.Tbl.create 64 in
  let dependencies = ID.Tbl.create 64 in
  let module VS = T.VarSet in

  let find_def lhs rhs =
    let try_extracting lhs rhs =
      match T.view lhs with
      | T.Const id ->
        CCOpt.return_if (T.is_ground rhs) (id, rhs)
      | T.App(hd, args) when T.is_const hd ->
        if List.for_all T.is_var args then (
          let var_set = VS.of_list (List.map T.as_var_exn args) in
          if (VS.cardinal var_set == List.length args &&
            (T.Seq.subterms ~include_builtin:true ~include_app_vars:true rhs
            |> Iter.filter (T.is_app_var)
            |> Iter.is_empty) &&
            Iter.length (T.Seq.vars rhs) == VS.cardinal (T.vars rhs))  then (
            Some (T.head_exn hd, rhs)
          ) else None
        ) else None
      | _ -> None
    in
    let (<+>) = CCOpt.(<+>) in
    try_extracting lhs rhs <+> try_extracting rhs lhs
  in

  let exception Loop in
  let topological_sort ~all_nodes dependencies = 
    let unvisited = ref (ID.Set.of_iter (ID.Tbl.keys dependencies)) in
    let visiting = ref ID.Set.empty in
    let visited = ref ID.Set.empty in
    let sorted = ref [] in

    let rec visit id =
      if not (ID.Set.mem id !visited) then (
        if ID.Set.mem id !visiting then raise Loop;

        visiting := ID.Set.add id !visiting;
        List.iter visit (ID.Tbl.get_or ~default:[] dependencies id);

        visited := ID.Set.add id !visited;
        visiting := ID.Set.remove id !visiting;
        unvisited := ID.Set.remove id !unvisited;
        sorted := id :: !sorted
      )
    in

    while (not (ID.Set.is_empty !unvisited)) do
      visit (ID.Set.choose !unvisited) 
    done;

    Util.debugf ~section 1 "top sorted: @[%a@]@." (fun k -> k (CCList.pp ID.pp) !sorted);

    let no_deps = ID.Set.to_list (ID.Set.diff all_nodes (ID.Set.of_list !sorted)) in
    no_deps @ !sorted
  in

  Iter.iter (fun cl -> 
    if Iter.length cl == 1 then (
      Util.debugf ~section 2 "working on %a" (fun k -> k (Iter.pp_seq (SLiteral.pp T.pp)) cl);
      match Iter.head_exn cl with
      | SLiteral.Eq(lhs, rhs) ->
        begin match find_def lhs rhs with
        | Some(hd_id, r) ->
          Util.debugf ~section 2 "is_def: %a := %a" (fun k -> k T.pp lhs T.pp rhs);
          ID.Tbl.update definition_map ~f:(fun _ -> 
            function None -> Some [r]
                    | Some res -> Some (r :: res)
            ) ~k:hd_id;
          Term.Seq.symbols r
          |> Iter.iter (fun k -> 
            ID.Tbl.update dependencies 
                          ~f:(fun _ -> function 
                                None -> Some [hd_id]
                                | Some res -> Some (hd_id :: res))
                          ~k)
        | _ -> (Util.debugf ~section 2 "is not def: %a := %a" (fun k -> k T.pp lhs T.pp rhs);) end
      | _ -> ()
    )
  ) clauses;

  let weights = ID.Tbl.create 64 in

  let eval_weight ~weights t =
    let rec aux t =
      match Term.view t with
      | Term.DB _ -> Weight.int db_w
      | Term.Var _ -> Weight.one
      | Term.Const id ->
      ID.Tbl.get_or weights id ~default:(base_weight id)
      | Term.Fun(_,body) -> (Weight.(+)) (Weight.int lm_w) (aux body)
      | Term.App(hd, args) -> aux_l (hd :: args)
      | Term.AppBuiltin(hd, args) ->
        (Weight.(+))
          (if (Builtin.is_quantifier hd) then Weight.omega else Weight.one)
          (aux_l args)
    and aux_l = function [] -> Weight.zero
      | [x] -> aux x
      | l -> List.fold_left (fun acc t -> 
        (Weight.(+)) acc (aux t)
      ) Weight.zero l
    in
    (Weight.(+)) (Weight.mult_one 2 (aux t)) (Weight.one)
  in


  (try


    (topological_sort ~all_nodes:(ID.Set.of_iter (ID.Tbl.keys definition_map))  
                      dependencies)
    |> CCList.iter (fun id ->
      let w_opt = Iter.max ~lt:(fun x y -> Weight.compare x y < 0)
        (Iter.map (eval_weight ~weights) 
          (Iter.of_list @@ ID.Tbl.get_or ~default:[] definition_map id)) in
      match w_opt with
      | Some w -> 
        Util.debugf ~section 1 "lambda weight lift of %a = %a" (fun k -> k ID.pp id Weight.pp w);
        ID.Tbl.add weights id w
      | None -> ()
    )
  with Loop -> (Util.debugf ~section 1 "warning looped" (fun k -> k); ));

  fun sy ->
    if is_post_cnf_skolem ~sig_ref:(ref empty_sig) sy then default_weight
    else (ID.Tbl.get_or ~default:(base_weight sy) weights sy)
  

let weight_fun_of_string ~signature ~clauses ~lm_w ~db_w s sd = 
  let syms_only sym_depth = 
    Iter.map fst sym_depth in
  let with_syms f sym_depth = f (syms_only sym_depth) in
  let ignore_arg f _ = f in 

  let wf_map = 
    ["invfreq", with_syms weight_invfreq; 
     "freq", with_syms weight_freq; 
     "invfreqrank", with_syms weight_invfreqrank;
     "freqrank", with_syms weight_freqrank;
     "modarity", ignore_arg @@ weight_modarity ~signature;
     "arity0", ignore_arg @@ weight_arity0 ~signature;
     "invarity", ignore_arg @@ weight_invarity ~signature;
     "sqarity", ignore_arg @@ weight_sq_arity ~signature;
     "invsqarity", ignore_arg @@ weight_invsq_arity ~signature;
     "invdocc", inv_depth_occurrence;
     "docc", depth_occurrence;
     "const", ignore_arg weight_constant] in
  try
    begin match CCString.chop_prefix ~pre:"lambda-def-" s with 
    | Some s ->
      let base_weight = List.assoc s wf_map sd in
      lambda_def_weight lm_w db_w base_weight clauses
      (* List.assoc s wf_map sd *)
    | None -> List.assoc s wf_map sd end
  with Not_found -> invalid_arg "KBO weight function not found"

(* default argument coefficients *)
let arg_coeff_default _ = []

let set_weight p f = p.weight <- f

(** {2 Creation of a precedence from constraints} *)

(* check invariant: the list is sorted w.r.t constraints *)
let check_inv_ p =
  let rec sorted_ = function
    | [] | [_] -> true
    | s :: ((s' :: _) as tail) ->
      assert (not (ID.equal s s'));
      p.constr s s' < 0
      &&
      sorted_ tail
  in
  sorted_ p.snapshot

let create ?(weight=weight_constant) ?(arg_coeff=arg_coeff_default)
    ?(db_w=db_w_def) ?(lmb_w=lmb_w_def)
    c l =
  let l = CCList.sort_uniq ~cmp:c l in
  let tbl = lazy (mk_tbl_ l) in
  let res = {
    snapshot=l;
    tbl;
    weight;
    arg_coeff;
    db_w;
    lmb_w;
    status=ID.Tbl.create 16;
    constr=c;
  } in
  assert (check_inv_ res);
  res

let add_list ~signature p l =
  (* sorted insertion in snapshot *)
  (* Signal.send on_signature_update signature; *)
  let rec insert_ id l = match l with
    | [] -> [id]
    | id' :: l' ->
      let c = p.constr id id' in
      if c=0 then (
        assert (ID.equal id id'); (* total order *)
        l  (* not new *)
      )
      else if c<0 then id :: l
      else
        let l' = insert_ id l' in
        id' :: l'
  in
  (* compute new snapshot, but only update precedence if any of the symbols is new *)
  let mk_snapshot l =
    List.fold_left
      (fun (snap) id ->
         insert_ id snap)
      (p.snapshot) l
  in
  let is_new_sym id =
    if Lazy.is_val p.tbl then ID.Tbl.mem (Lazy.force p.tbl) id
    else List.mem id p.snapshot
  in

  let l = List.filter is_new_sym l in
  let cutoff = 30 in
  let snapshot = 
    if CCList.length l < cutoff then mk_snapshot l
    else (List.fast_sort p.constr ( p.snapshot @ l))
  in

  if not (CCList.is_empty l) then (
    Util.debugf ~section 4 "@[<v>old prec: @[%a@]@,new prec: @[%a@]@]"
      (fun k->k (Util.pp_list ID.pp) p.snapshot (Util.pp_list ID.pp) snapshot);
    assert (check_inv_ p);
    p.snapshot <- snapshot;
    p.tbl <- lazy (mk_tbl_ snapshot);
  )

let add p id = add_list p [id]

let default l = create Constr.alpha l

let default_seq seq =
  default (Iter.to_rev_list seq)

let constr p = p.constr
