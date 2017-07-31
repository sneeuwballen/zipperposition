
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Constraint Solving for LPO} *)

open Logtk

module SI = Msat.Solver_intf

let section = Util.Section.(make ~parent:(make "solving") "lpo")

(** {6 Constraints} *)

module Constraint = struct
  type expr = ID.t

  type t =
    | EQ of expr * expr
    | LE of expr * expr
    | LT of expr * expr
    | And of t list
    | Or of t list
    | Not of t
    | True   (* tautology *)
    | False  (* impossible constraint *)


  let eq a b = EQ (a,b)
  let neq a b = Not (EQ (a, b))
  let le a b = LE (a,b)
  let lt a b = LT (a,b)
  let ge a b = LE (b,a)
  let gt a b = LT (b,a)
  let and_ l = And l
  let or_ l = Or l
  let not_ t = Not t
  let imply a b = Or [Not a; b]
  let true_ = True
  let false_ = False

  module Seq = struct
    let exprs c k =
      let rec iter c = match c with
        | EQ (a,b) | LE(a,b) | LT(a,b) -> k a; k b
        | And l | Or l -> List.iter iter l
        | Not t -> iter t
        | True | False -> ()
      in
      iter c
  end

  let pp_expr = ID.pp

  let rec pp out t = match t with
    | EQ (a,b) -> Format.fprintf out "(= %a %a)" pp_expr a pp_expr b
    | LE (a,b) -> Format.fprintf out "(<= %a %a)" pp_expr a pp_expr b
    | LT (a,b) -> Format.fprintf out "(< %a %a)" pp_expr a pp_expr b
    | And l -> Format.fprintf out "(and %a)" (Util.pp_list ~sep:" " pp) l
    | Or l -> Format.fprintf out "(or %a)" (Util.pp_list ~sep:" " pp) l
    | Not t -> Format.fprintf out "(not %a)" pp t
    | True -> CCFormat.string out "true"
    | False -> CCFormat.string out "false"

  let to_string = CCFormat.to_string pp

  (* simplify the constraints *)
  let rec simplify t =
    match t with
      | EQ (a, b) when ID.equal a b -> true_
      | LE (a, b) when ID.equal a b -> true_
      | LT (a, b) when ID.equal a b -> false_
      | Not (Not t) -> simplify t
      | Not True -> true_
      | Not False -> true_
      | Not (And l) -> simplify (or_ (List.map not_ l))
      | Not (Or l) -> simplify (and_ (List.map not_ l))
      | And [] -> true_
      | Or [] -> true_
      | And [x] -> simplify x
      | Or [x] -> simplify x
      | And l ->
        let l' = List.fold_left flatten_and [] l in
        begin match l' with
          | [] -> true_
          | _ when List.mem false_ l' -> false_
          | [x] -> x
          | _ -> and_ l'
        end
      | Or l ->
        let l' = List.fold_left flatten_or [] l in
        begin match l' with
          | [] -> false_
          | _ when List.mem true_ l' -> true_
          | [x] -> x
          | _ -> or_ l'
        end
      | _ -> t
  and flatten_or acc t = match simplify t with
    | False -> acc
    | Or l -> List.fold_left flatten_or acc l
    | t' -> t' :: acc
  and flatten_and acc t = match simplify t with
    | True -> acc
    | And l -> List.fold_left flatten_and acc l
    | t' -> t' :: acc
end

module Solution = struct
  type t = (ID.t * ID.t) list

  (* constraint that prohibits this solution. We build the
     clause that makes at least one a>b false. *)
  let neg_to_constraint sol =
    let module C = Constraint in
    let l = List.map (fun (a,b) -> C.le a b) sol in
    C.or_ l

  let pp out s =
    Util.pp_list (Util.pp_pair ~sep:" > " ID.pp ID.pp) out s

  let to_string = CCFormat.to_string pp
end

module C = Constraint

(** Functor to use Sat, and encode/decode the solution.
    Use "Solving Partial Order Constraints for LPO Termination", Codish & al *)
module MakeSolver(X : sig end) = struct
  module Lit = struct
    type t = int
    let fresh = let n = ref 0 in fun () -> incr n; !n
    let sign x = x>0
    let abs = abs
    let print = Format.pp_print_int
    let dummy = 0
    let neg i = -i
    let hash i = i land max_int
    let equal i j = i=j
    let norm i =
      if i>0 then i, Msat.Formula_intf.Same_sign
      else -i, Msat.Formula_intf.Negated
    type proof = unit
  end

  module Solver = Msat.Solver.Make(Lit)(Msat.Solver.DummyTheory(Lit))(struct end)

  (* propositional atoms map symbols to the binary digits of
     their index in the precedence *)
  module Atom : sig
    type t
    val make : ID.t -> int -> t
    val equal : t -> t -> bool
    val hash : t -> int
    val print : Format.formatter -> t -> unit
  end = struct
    type t = ID.t * int
    let make s i = s, i
    let equal (s1,i1)(s2,i2) = ID.equal s1 s2 && i1 = i2
    let hash (s,i) = Hash.combine3 42 (ID.hash s) (Hash.int i)
    let print fmt (s,i) = Format.fprintf fmt "%a/%d" ID.pp s i
  end

  module AtomTbl = CCHashtbl.Make(Atom)

  let atom_to_int_ = AtomTbl.create 16
  let int_to_atom_ = Hashtbl.create 16

  (* unique "literal" (int) for this atom *)
  let atom_to_lit a =
    try
      AtomTbl.find atom_to_int_ a
    with Not_found ->
      let i = Lit.fresh () in
      AtomTbl.add atom_to_int_ a i;
      Hashtbl.add int_to_atom_ i a;
      i

  (* get the propositional variable that represents the n-th bit of [s] *)
  let digit s n = atom_to_lit (Atom.make s n)

  module F = Msat.Tseitin.Make(Lit)

  (* encode [a < b]_n where [n] is the number of digits.
      either the n-th digit of [a] is false and the one of [b] is true,
      or they are equal and [a < b]_{n-1}.
      @return a formula *)
  let rec encode_lt ~n a b =
    if n = 0 then F.f_false
    else
      let d_a = F.make_atom (digit a n)
      and d_b = F.make_atom (digit b n) in
      F.make_or
        [ F.make_and [ F.make_not d_a; d_b ]
        ; F.make_and [ F.make_equiv d_a d_b; encode_lt ~n:(n-1) a b]
        ]

  (* encode [a <= b]_n, with not [b < a]_n. *)
  let encode_leq ~n a b =
    F.make_not (encode_lt ~n b a)

  (* encode [a = b]_n, digit per digit *)
  let rec encode_eq ~n a b =
    if n = 0 then F.f_true
    else
      let d_a = F.make_atom (digit a n)
      and d_b = F.make_atom (digit b n) in
      F.make_and [ F.make_equiv d_a d_b; encode_eq ~n:(n-1) a b ]

  (* encode a constraint with [n] bits into a formula. *)
  let rec encode_constr ~n c = match c with
    | C.EQ(a,b) -> encode_eq ~n a b
    | C.LE(a,b) -> encode_leq ~n a b
    | C.LT(a,b) -> encode_lt ~n a b
    | C.And l -> F.make_and (List.rev_map (encode_constr ~n) l)
    | C.Or l -> F.make_or (List.rev_map (encode_constr ~n) l)
    | C.Not c' -> F.make_not (encode_constr ~n c')
    | C.True -> F.f_true
    | C.False -> F.f_false

  (* function to extract symbol -> int from a solution *)
  let int_of_symbol sat ~n s =
    let r = ref 0 in
    for i = n downto 1 do
      let lit = digit s i in
      let is_true = sat.SI.eval lit in
      if is_true
      then r := 2 * !r + 1
      else r := 2 * !r
    done;
    Util.debugf ~section 3 "index of symbol %a in precedence is %d" (fun k->k ID.pp s !r);
    !r

  (* extract a solution *)
  let get_solution sat ~n symbols =
    let syms = List.rev_map (fun s -> int_of_symbol sat ~n s, s) symbols in
    (* sort in increasing order *)
    let syms = List.sort (fun (n1,_)(n2,_) -> n1-n2) syms in
    (* build solution by imposing f>g iff n(f) > n(g) *)
    let _, _, sol = List.fold_left
        (fun (cur_n,other_s,acc) (n,s) ->
           if n = cur_n
           then n, s::other_s, acc   (* yet another symbol with rank [n] *)
           else begin
             (* elements of [other_s] have a lower rank, force them to be smaller  *)
             assert (cur_n < n);
             let acc = List.fold_left
                 (fun acc s' -> (s, s') :: acc)
                 acc other_s
             in n, [s], acc
           end
        ) (~-1,[],[]) syms
    in
    sol

  let print_lit fmt i =
    if not (Lit.sign i) then Format.fprintf fmt "¬";
    try
      let a = Hashtbl.find int_to_atom_ (Lit.abs i) in
      Atom.print fmt a
    with Not_found ->
      Format.fprintf fmt "L%d" (abs (i : Solver.atom :> int))  (* tseitin *)

  let print_clause fmt c =
    Format.fprintf fmt "@[<hv2>%a@]"
      (Util.pp_list ~sep:" or " print_lit) c

  let print_clauses fmt c =
    Format.fprintf fmt "@[<v>%a@]" (Util.pp_list ~sep:"" print_clause) c

  (* solve the given list of constraints *)
  let solve_list l =
    (* count the number of symbols *)
    let symbols =
      Sequence.of_list l
      |> Sequence.flat_map C.Seq.exprs
      |> ID.Set.of_seq
      |> ID.Set.elements
    in
    let num = List.length symbols in
    (* the number of digits required to map each symbol to a distinct int *)
    let n = int_of_float (ceil (log (float_of_int num) /. log 2.)) in
    Util.debugf ~section 2 "constraints on %d symbols -> %d digits (%d bool vars)"
      (fun k->k num n (n * num));
    let encode_constr c =
      Util.debugf ~section 5 "encode constr %a..." (fun k->k C.pp c);
      let f = encode_constr ~n c in
      Util.debugf ~section 5 " ... @[<2>%a@]" (fun k->k F.print f);
      let clauses = F.make_cnf f in
      Util.debugf ~section 5 " ... @[<0>%a@]" (fun k->k print_clauses clauses);
      Solver.assume clauses;
      Util.debug ~section 5 "form assumed"
    in
    List.iter encode_constr l;
    (* generator of solutions *)
    let rec next () =
      Util.debug ~section 5 "check satisfiability";
      match Solver.solve () with
        | Solver.Sat sat ->
          Util.debug ~section 5 "next solution exists, try to extract it...";
          let solution = get_solution sat ~n symbols in
          Util.debugf ~section 5 "... solution is %a" (fun k->k Solution.pp solution);
          (* obtain another solution: negate current one and continue *)
          let tl = lazy (negate ~n solution) in
          LazyList.Cons (solution, tl)
        | Solver.Unsat _ ->
          Util.debug ~section 5 "no solution";
          LazyList.Nil
    and negate ~n:_ solution =
      (* negate current solution to get the next one... if any *)
      let c = Solution.neg_to_constraint solution in
      encode_constr c;
      match Solver.solve () with
        | Solver.Sat _ -> next()
        | Solver.Unsat _ -> LazyList.Nil
    in
    lazy (next())
end

let solve_multiple l =
  let l = List.rev_map C.simplify l in
  Util.debugf ~section 2 "lpo: solve constraints %a" (fun k->k (CCFormat.list C.pp) l);
  let module S = MakeSolver(struct end) in
  S.solve_list l

(** {6 LPO} *)

(* constraint that some term in [l] is bigger than [b] *)
let any_bigger ~orient_lpo l b  = match l with
  | [] -> C.false_
  | [x] -> orient_lpo x b
  | _ -> (* any element of [l] bigger than [r]? *)
    C.or_ (List.rev_map (fun x -> orient_lpo x b) l)

(* [a] bigger than all the elements of [l] *)
and all_bigger ~orient_lpo a l = match l with
  | [] -> C.true_
  | [x] -> orient_lpo a x
  | _ ->
    C.and_ (List.rev_map (fun y -> orient_lpo a y) l)

(* constraint for l1 >_lex l2 (lexicographic extension of LPO) *)
and lexico_order ~eq ~orient_lpo l1 l2 =
  assert (List.length l1 = List.length l2);
  let c = List.fold_left2
      (fun constr a b ->
         match constr with
           | Some _ -> constr
           | None when eq a b -> None
           | None -> Some (orient_lpo a b))
      None l1 l2
  in match c with
    | None -> C.false_   (* they are equal *)
    | Some c -> c

module FO = struct
  module T = Term
  type term = T.t
  module TC = T.Classic

  (* constraint for a > b *)
  let rec orient_lpo a b =
    match TC.view a, TC.view b with
      | (TC.Var _ | TC.DB _), _ ->
        C.false_  (* a variable cannot be > *)
      | _, _ when T.subterm ~sub:b a ->
        C.true_  (* trivial subterm property --> ok! *)
      | TC.App (f, ((_::_) as l)), TC.App (g, l')
        when List.length l = List.length l' ->
        (* three cases: either some element of [l] is > [r],
            or precedence of first symbol applies,
            or lexicographic case applies (with non empty lists) *)
        C.or_
          [ C.and_
              [ C.eq f g
              ; lexico_order ~eq:T.equal ~orient_lpo l l' ]
          (* f=g, lexicographic order of subterms *)
          ; C.and_
              [ C.gt f g
              ; all_bigger ~orient_lpo a l'
              ]  (* f>g and a > all subterms of b *)
          ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
          ]
      | TC.App (f, l), TC.App (g, l') ->
        (* two cases: either some element of [l] is > [r],
            or precedence of first symbol applies *)
        C.or_
          [ C.and_
              [ C.gt f g
              ; all_bigger ~orient_lpo a l'
              ]  (* f>g and a > all subterms of b *)
          ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
          ]
      | TC.App (_, l), _ ->
        (* only the subterm property can apply *)
        any_bigger ~orient_lpo l b
      | TC.AppBuiltin _, _
      | _, TC.AppBuiltin _
      | TC.NonFO, _ ->
        (* no clue... *)
        C.false_

  let orient_lpo_list l =
    List.map
      (fun (l,r) ->
         let c = orient_lpo l r in
         let c' = C.simplify c in
         Util.debugf ~section 2 "constr %a simplified into %a" (fun k->k C.pp c C.pp c');
         c')
      l
end

module TypedSTerm = struct
  module T = TypedSTerm
  type term = T.t

  (* constraint for a > b *)
  let rec orient_lpo a b =
    match T.view a, T.view b with
      | T.Var _ , _ ->
        C.false_  (* a variable cannot be > *)
      | _ when T.equal a b -> C.false_
      | _ when T.is_subterm ~strict:true b ~of_:a ->
        C.true_  (* trivial subterm property --> ok! *)
      | T.App (f, l), T.App (g, l') ->
        begin match T.view f, T.view g with
          | T.Const f, T.Const g when List.length l = List.length l' ->
            (* three cases: either some element of [l] is > [r],
                or precedence of first symbol applies,
                or lexicographic case applies (with non empty lists) *)
            C.or_
              [ C.and_
                  [ C.eq f g
                  ; lexico_order ~eq:T.equal ~orient_lpo l l' ]  (* f=g, lexicographic order of subterms *)
              ; C.and_
                  [ C.gt f g
                  ; all_bigger ~orient_lpo a l'
                  ]  (* f>g and a > all subterms of b *)
              ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
              ]
          | T.Const f, T.Const g ->
            (* two cases: either some element of [l] is > [r],
                or precedence of first symbol applies *)
            C.or_
              [ C.and_
                  [ C.gt f g
                  ; all_bigger ~orient_lpo a l'
                  ]  (* f>g and a > all subterms of b *)
              ; any_bigger ~orient_lpo l b  (* some subterm of a is > b *)
              ]
          | _ -> C.false_ (* no clue *)
        end
      | T.App (f, l), _ when T.is_const f ->
        (* only the subterm property can apply *)
        any_bigger ~orient_lpo l b
      | _ ->
        (* no clue... *)
        C.false_

  let orient_lpo_list l =
    List.map
      (fun (l,r) ->
         let c = orient_lpo l r in
         let c' = C.simplify c in
         Util.debugf ~section 2 "constr %a simplified into %a" (fun k->k C.pp c C.pp c');
         c')
      l
end
