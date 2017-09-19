
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Precedence (total ordering) on symbols} *)

type symbol_status =
  | Multiset
  | Lexicographic
  | LengthLexicographic

let section = Util.Section.(make "precedence")

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

module Constr = struct
  type 'a t = ID.t -> ID.t -> int
    constraint 'a = [< `partial | `total]

  let arity arity_of s1 s2 =
    (* bigger arity means bigger symbol *)
    arity_of s1 - arity_of s2

  let invfreq seq =
    (* symbol -> number of occurrences of symbol in seq *)
    let tbl = ID.Tbl.create 16 in
    Sequence.iter (ID.Tbl.incr tbl) seq;
    let find_freq s = ID.Tbl.get_or ~default:0 tbl s in
    (* compare by inverse frequency (higher frequency => smaller) *)
    fun s1 s2 ->
      let n1 = find_freq s1 in
      let n2 = find_freq s2 in
      CCInt.compare n2 n1

  let max l =
    let set = ID.Set.of_list l in
    fun s1 s2 ->
      let is_max1 = ID.Set.mem s1 set in
      let is_max2 = ID.Set.mem s2 set in
      match is_max1, is_max2 with
        | true, true
        | false, false -> 0
        | true, false -> 1
        | false, true -> -1

  let min l =
    let set = ID.Set.of_list l in
    fun s1 s2 ->
      let is_min1 = ID.Set.mem s1 set in
      let is_min2 = ID.Set.mem s2 set in
      match is_min1, is_min2 with
        | true, true
        | false, false -> 0
        | true, false -> -1
        | false, true -> 1

  (* regular string ordering *)
  let alpha a b =
    let c = String.compare (ID.name a) (ID.name b) in
    if c = 0
    then ID.compare a b else c

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
  constr : [`total] Constr.t;
  (* constraint used to build and update the precedence *)
}

type precedence = t

let equal p1 p2 =
  try List.for_all2 ID.equal p1.snapshot p2.snapshot
  with Invalid_argument _ -> false

let snapshot p = p.snapshot

let compare_by_tbl p s1 s2 =
  let lazy tbl = p.tbl in
  let i1 = ID.Tbl.get_or ~default:~-1 tbl s1 in
  let i2 = ID.Tbl.get_or ~default:~-1 tbl s2 in
  let c = CCInt.compare i1 i2 in
  if c = 0
  then (
    (* Format.printf "%a (%d) and %a (%d)@." ID.pp_full s1 i1 ID.pp_full s2 i2; *)
    assert (ID.equal s1 s2);
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

let arg_coeff p s i = try List.nth (p.arg_coeff s) i with _ -> 1

let declare_status p s status =
  ID.Tbl.replace p.status s status

module Seq = struct
  let symbols p = Sequence.of_list p.snapshot
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

(* weight of f = arity of f + 4 *)
let weight_modarity ~arity a = Weight.int (arity a + 4)

(* constant weight *)
let weight_constant _ = Weight.int 4

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

let create ?(weight=weight_constant) ?(arg_coeff=arg_coeff_default) c l =
  let l = CCList.sort_uniq ~cmp:c l in
  let tbl = lazy (mk_tbl_ l) in
  let res = {
    snapshot=l;
    tbl;
    weight;
    arg_coeff;
    status=ID.Tbl.create 16;
    constr=c;
  } in
  assert (check_inv_ res);
  res

let add_list p l =
  (* sorted insertion in snapshot *)
  let rec insert_ id l = match l with
    | [] -> [id], true
    | id' :: l' ->
      let c = p.constr id id' in
      if c=0 then (
        assert (ID.equal id id'); (* total order *)
        l, false  (* not new *)
      )
      else if c<0 then id :: l, true
      else
        let l', ret = insert_ id l' in
        id' :: l', ret
  in
  (* compute new snapshot, but only update precedence if any of the symbols is new *)
  let snapshot, is_new =
    List.fold_left
      (fun (snap,new_) id ->
         let snap,new_' = insert_ id snap in
         snap, new_ || new_')
      (p.snapshot,false) l
  in
  if is_new then (
    Util.debugf ~section 4 "@[<v>old prec: @[%a@]@,new prec: @[%a@]@]"
      (fun k->k (Util.pp_list ID.pp) p.snapshot (Util.pp_list ID.pp) snapshot);
    assert (check_inv_ p);
    p.snapshot <- snapshot;
    p.tbl <- lazy (mk_tbl_ snapshot);
  )

let add p id = add_list p [id]

let add_seq p seq = Sequence.iter (add p) seq

let default l = create Constr.alpha l

let default_seq seq =
  default (Sequence.to_rev_list seq)

let constr p = p.constr
