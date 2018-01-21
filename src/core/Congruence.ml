
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Simple and Lightweight Congruence and order} *)

module type S = Congruence_intf.S

(** The graph used for the congruence *)

module type TERM = sig
  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val subterms : t -> t list
  (** Subterms of the term (possibly empty list) *)

  val update_subterms : t -> t list -> t
  (** Replace immediate subterms by the given list.
      This is used to test for equality *)

  val pp : t CCFormat.printer
end

module Make(T : TERM) = struct
  type term = T.t

  module H = CCPersistentHashtbl.Make(struct
      type t = term
      let equal = T.equal
      let hash = T.hash
    end)

  (** Maps terms to their list of immediate parents, and current
      representative *)
  type t = {
    parents: term list H.t; (* parent terms *)
    mutable next: term H.t; (* pointer towards representative *)
  }

  let create ?(size=64) () = {
    parents=H.create size;
    next=H.create size;
  }

  (* update [node.next] to be [next] *)
  let[@inline] set_next_ cc t next : t =
    { cc with next=H.replace cc.next t next }

  (* update [node.parents] to be [parents] *)
  let[@inline] set_parents_ cc t parents : t =
    { cc with parents=H.replace cc.parents t parents }

  let[@inline] next_ cc t = H.get t cc.next |> CCOpt.get_or ~default:t
  let[@inline] parents_ cc t = H.get t cc.parents |> CCOpt.get_or ~default:[]

  (* find representative *)
  let rec find_ cc (t:term) : term =
    let next = next_ cc t in
    if T.equal t next
    then t  (* root *)
    else (
      let root = find_ cc next in
      (* path compression. Can be done in place as it doesn't change
         the semantics of the CC *)
      if not (T.equal root next) then (
        cc.next <- H.replace cc.next t root;
      );
      root
    )

  (* are two nodes, with their subterm lists, congruent? To
      check this, we compute the representative of subnodes
      and we check whether updated subterms are equal *)
  let are_congruent_ cc t1 t2 =
    let l1' = List.map (find_ cc) (T.subterms t1) in
    let l2' = List.map (find_ cc) (T.subterms t2) in
    try
      let t1 = T.update_subterms t1 l1' in
      let t2 = T.update_subterms t2 l2' in
      T.equal t1 t2
    with Type.ApplyError _ ->
      false

  (* merge n1 and n2 equivalence classes *)
  let rec merge_ cc (t1:term) (t2:term) : t =
    (* get representatives *)
    let t1 = find_ cc t1 in
    let t2 = find_ cc t2 in
    if T.equal t1 t2 then cc
    else (
      let left, right = parents_ cc t1, parents_ cc t2 in
      (* n1 now points to n2, put every class information in n2 *)
      let cc = set_next_ cc t1 t2 in
      let cc = set_parents_ cc t2 (List.rev_append left right) in
      (* check congruence of parents of n1 and n2 *)
      List.fold_left
        (fun cc p1 ->
           List.fold_left
             (fun cc p2 ->
                if not (T.equal p1 p2) && are_congruent_ cc p1 p2 then (
                  merge_ cc p1 p2
                ) else cc)
             cc right)
        cc left
    )

  (* add [t] to the CC *)
  let rec add cc (t:term) : t =
    if H.mem cc.parents t then (
      assert (H.mem cc.next t);
      cc
    ) else (
      let subs = T.subterms t in
      let cc = set_parents_ cc t [] in
      let cc = set_next_ cc t t in
      (* add subterms *)
      let cc = List.fold_left add cc subs in
      (* add [t] to list of subterms *)
      let cc =
        List.fold_left
          (fun cc sub ->
             let repr = find_ cc sub in
             set_parents_ cc repr (t :: parents_ cc repr))
          cc
          subs
      in
      let cc =
        List.fold_left
          (fun cc sub ->
             let parents = find_ cc sub |> parents_ cc in
             List.fold_left
               (fun cc parent_sub ->
                  if not (T.equal t parent_sub) &&
                     are_congruent_ cc t parent_sub then (
                    merge_ cc t parent_sub
                  ) else cc)
               cc parents)
          cc
          subs
      in
      cc
    )

  let iter cc f =
    H.iter cc.next
      (fun mem _ ->
         let repr = find_ cc mem in
         f ~mem ~repr)

  let iter_roots cc f =
    H.iter cc.next
      (fun t next -> if T.equal t next then f t)

  let[@inline] mk_eq cc t1 t2 =
    let cc = add cc t1 in
    let cc = add cc t2 in
    merge_ cc t1 t2

  let[@inline] is_eq cc t1 t2 =
    let cc = add cc t1 in
    let cc = add cc t2 in
    T.equal (find_ cc t1) (find_ cc t2)

  let pp_debug out (cc:t) : unit =
    let module Fmt = CCFormat in
    let pp_parent out (t,l) =
      Fmt.fprintf out "(@[<hv1>parents@ :of %a@ (@[<v>%a@])@])"
        T.pp t (Util.pp_list ~sep:" " T.pp) l
    and pp_next out (t,u) =
      Fmt.fprintf out "(@[<hv>next@ :of %a@ :is %a@])" T.pp t T.pp u
    in
    Fmt.fprintf out "(@[<v>cc@ :parent_tbl (@[<v>%a@])@ :next_tbl (@[<v>%a@])@])"
      (Util.pp_seq pp_parent) (H.to_seq cc.parents)
      (Util.pp_seq pp_next) (H.to_seq cc.next)
end

module FO = Make(struct
    module T = Term

    type t = T.t
    let equal = T.equal
    let hash = T.hash
    let pp = T.pp

    let subterms t = match T.Classic.view t with
      | T.Classic.App (_, l) -> l
      | _ -> []

    let update_subterms t l = match T.view t, l with
      | T.App (hd, l), l' when List.length l = List.length l' ->
        T.app hd l'
      | _, [] -> t
      | _ -> assert false
  end)
