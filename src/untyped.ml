
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Terms without type, typically produced from AST} *)

(** {2 First Order terms} *)

module FO = struct
  type t =
    | App of Symbol.t * t list
    | Var of string * Type.Parsed.t

  let eq a b = a = b
  let cmp a b = Pervasives.compare a b
  let hash a = Hashtbl.hash a

  let app s l = App (s, l)
  let const s = app s []
  let var ~ty s = Var (s,ty)
  
  let symbols seq =
    let rec recurse set t = match t with
    | App (s, l) ->
      let set = Symbol.Set.add s set in
      List.fold_left recurse set l
    | Var _ -> set
    in
    Sequence.fold recurse Symbol.Set.empty seq

  let free_vars ?(init=[]) t =
    let rec find set t = match t with
      | Var _ -> if List.mem t set then set else t :: set
      | App (_, l) -> List.fold_left find set l
    in
    find init t

  let _same_name v1 v2 = match v1, v2 with
    | Var (n1, _), Var (n2, _) -> n1 = n2
    | _ -> false

  (* replace var of given [name] by [v] in [t] *)
  let rec _replace_var v t = match t with
    | Var _ ->
      if _same_name v t then v else t
    | App (s, l) ->
      let l = List.map (_replace_var v) l in
      app s l

  let rec pp buf t = match t with
    | App (s, []) -> Symbol.pp buf s
    | App (s, l) ->
      Printf.bprintf buf "%a(%a)" Symbol.pp s (Util.pp_list pp) l
    | Var (s, ty) ->
      begin match ty with
      | Type.Parsed.App ("$i",[]) -> Printf.bprintf buf "%s" s
      | _ -> Printf.bprintf buf "%s:%a" s Type.Parsed.pp ty
      end

  let pp_tstp = pp
  let to_string = Util.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

(** {2 First Order formulas} *)

module Form = struct
  type b_op =
    | Imply
    | Equiv

  type l_op =
    | And
    | Or

  type q_op =
    | Forall
    | Exists

  type t =
    | Nary of l_op * t list
    | Binary of b_op * t * t
    | Not of t
    | Bool of bool
    | Equal of FO.t * FO.t
    | Atom of FO.t
    | Quant of q_op * FO.t list * t

  type sourced = t * string * string
    (** Sourced formula *)

  let eq a b = a = b
  let cmp a b = Pervasives.compare a b
  let hash a = Hashtbl.hash a

  let mk_and l = Nary (And, l)
  let mk_or l = Nary (Or, l)
  let mk_not t = Not t
  let mk_eq t1 t2 = Equal (t1, t2)
  let mk_neq t1 t2 = mk_not (mk_eq t1 t2)
  let mk_equiv f1 f2 = Binary (Equiv, f1, f2)
  let mk_xor f1 f2 = mk_not (mk_equiv f1 f2)
  let mk_imply f1 f2 = Binary (Imply, f1, f2)
  let atom t = Atom t
  let mk_true = Bool true
  let mk_false = Bool false

  (* replace vars with same name as [v] by [v] in [f] *)
  let rec _replace_var v f = match f with
    | Bool _ -> f
    | Not f' -> mk_not (_replace_var v f')
    | Binary (op, f1, f2) ->
      Binary (op, _replace_var v f1, _replace_var v f2)
    | Nary (op, l) ->
      Nary (op, List.map (_replace_var v) l)
    | Quant (op, vars, f') ->
      if List.exists (FO._same_name v) vars
        then f  (* shadowed *)
        else Quant (op, vars, _replace_var v f')
    | Equal (t1, t2) -> Equal (FO._replace_var v t1, FO._replace_var v t2)
    | Atom p -> Atom (FO._replace_var v p)

  (* be sure that all variables in [vars] are properly replace in [f] *)
  let _replace_vars vars f =
    assert (List.for_all (function | FO.Var _ -> true | _ -> false) vars);
    List.fold_left
      (fun f v -> _replace_var v f)
      f vars

  let forall vars f = Quant (Forall, vars, _replace_vars vars f)
  let exists vars f = Quant (Exists, vars, _replace_vars vars f)

  let free_vars f =
    let rec find set f = match f with
    | Bool _ -> set
    | Not f' -> find set f'
    | Binary (_, f1, f2) -> find (find set f1) f2
    | Nary (_, l) -> List.fold_left find set l
    | Atom p -> FO.free_vars ~init:set p
    | Equal (t1, t2) -> FO.free_vars ~init:(FO.free_vars ~init:set t1) t2
    | Quant (_, vars, f') ->
      let set' = find [] f' in
      let set' = List.filter (fun v -> not (List.mem v vars)) set' in
      Util.list_merge compare set set'
    in
    find [] f

  let close_forall f = match free_vars f with
    | [] -> f
    | vars -> forall vars f

  let close_exists f = match free_vars f with
    | [] -> f
    | vars -> exists vars f

  let rec pp buf f = match f with
    | Bool true -> Buffer.add_string buf "$true"
    | Bool false -> Buffer.add_string buf "$false"
    | Nary (And, l) -> Util.pp_list ~sep:" ∧ " pp_inner buf l
    | Nary (Or, l) -> Util.pp_list ~sep:" ∨ " pp_inner buf l
    | Atom t -> FO.pp buf t
    | Binary (Imply, f1, f2) ->
      Printf.bprintf buf "%a → %a" pp_inner f1 pp_inner f2
    | Binary (Equiv, f1, f2) ->
      Printf.bprintf buf "%a <=> %a" pp_inner f1 pp_inner f2
    | Equal (t1, t2) ->
      Printf.bprintf buf "%a = %a" FO.pp t1 FO.pp t2
    | Not (Equal (t1, t2)) ->
      Printf.bprintf buf "%a ≠ %a" FO.pp t1 FO.pp t2
    | Not (Binary (Equiv, f1, f2)) ->
      Printf.bprintf buf "%a <~> %a" pp_inner f1 pp_inner f2
    | Not f' -> Printf.bprintf buf "¬%a" pp_inner f'
    | Quant (Forall, vars, f') ->
      Printf.bprintf buf "∀%a. %a" (Util.pp_list FO.pp) vars pp_inner f'
    | Quant (Exists, vars, f') ->
      Printf.bprintf buf "∃%a. %a" (Util.pp_list FO.pp) vars pp_inner f'
  and pp_inner buf f = match f with
    | Bool _
    | Equal _
    | Quant _
    | Atom _
    | Not _ -> pp buf f
    | Nary _
    | Binary _ ->
      Buffer.add_char buf '(';
      pp buf f;
      Buffer.add_char buf ')'

  (* TODO: only print types in binders, not in subterms *)

  let rec pp_tstp buf f = match f with
    | Bool true -> Buffer.add_string buf "$true"
    | Bool false -> Buffer.add_string buf "$false"
    | Nary (And, l) -> Util.pp_list ~sep:" & " pp_tstp_inner buf l
    | Nary (Or, l) -> Util.pp_list ~sep:" | " pp_tstp_inner buf l
    | Atom t -> FO.pp_tstp buf t
    | Binary (Imply, f1, f2) ->
      Printf.bprintf buf "%a | %a" pp_tstp_inner f1 pp_tstp_inner f2
    | Binary (Equiv, f1, f2) ->
      Printf.bprintf buf "%a <=> %a" pp_tstp_inner f1 pp_tstp_inner f2
    | Equal (t1, t2) ->
      Printf.bprintf buf "%a = %a" FO.pp_tstp t1 FO.pp_tstp t2
    | Not (Equal (t1, t2)) ->
      Printf.bprintf buf "%a != %a" FO.pp_tstp t1 FO.pp_tstp t2
    | Not (Binary (Equiv, f1, f2)) ->
      Printf.bprintf buf "%a <~> %a" pp_tstp_inner f1 pp_tstp_inner f2
    | Not f' -> Printf.bprintf buf "~ %a" pp_tstp_inner f'
    | Quant (Forall, vars, f') ->
      Printf.bprintf buf "![%a]: %a" (Util.pp_list FO.pp_tstp) vars pp_tstp_inner f'
    | Quant (Exists, vars, f') ->
      Printf.bprintf buf "?[%a]: %a" (Util.pp_list FO.pp_tstp) vars pp_tstp_inner f'
  and pp_tstp_inner buf f = match f with
    | Bool _
    | Equal _
    | Quant _
    | Atom _
    | Not _ -> pp_tstp buf f
    | Nary _
    | Binary _ ->
      Buffer.add_char buf '(';
      pp_tstp buf f;
      Buffer.add_char buf ')'

  let to_string = Util.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end

(** {2 Higher order Terms} *)

module HO = struct
  type t =
    | Const of Symbol.t
    | App of t * t list
    | Var of string * Type.Parsed.t
    | Lambda of t * t

  let eq a b = a = b
  let cmp a b = Pervasives.compare a b
  let hash a = Hashtbl.hash a

  let const s = Const s

  let rec app a l = match a with
    | App (a', l') -> app a' (l' @ l)   (* no nested apps *)
    | _ ->
      begin match l with
      | [] -> a
      | _::_ -> App (a, l)
      end

  let at a b = app a [b]
  let var ~ty s = Var (s, ty)

  let _same_name v1 v2 = match v1, v2 with
    | Var (n1, _), Var (n2, _) -> n1 = n2
    | _ -> false

  (* replace vars with same name as [v] by [v] in [t] *)
  let rec _replace_var v t = match t with
    | Const _ -> t
    | Var _ ->
      if _same_name v t then v else t
    | App (t, l) ->
      let t = _replace_var v t in
      let l = List.map (_replace_var v) l in
      app t l
    | Lambda (Var _ as var, t') ->
      if _same_name v var
        then t  (* name is shadowed under quantifier *)
        else Lambda (var, _replace_var v t')
    | Lambda _ -> assert false

  let lambda ~var t = match var with
    | Var _  ->
      (* be sure that all variables occurring in [t] with the same name
          have the same type *)
      Lambda (var, _replace_var var t)
    | _ -> failwith "Untyped.HO.lambda: expect (var, term)"

  let true_term = const (Symbol.true_symbol)
  let false_term = const (Symbol.false_symbol)

  let forall ~var t = at (const Symbol.forall_symbol) (lambda ~var t)
  let exists ~var t = at (const Symbol.exists_symbol) (lambda ~var t)

  let rec forall_list vars t = match vars with
    | [] -> t
    | var::vars' -> forall ~var (forall_list vars' t)

  let rec exists_list vars t = match vars with
    | [] -> t
    | var::vars' -> exists ~var (exists_list vars' t)

  let rec of_term t = match t with
    | FO.Var (n, ty) -> var ~ty n
    | FO.App (s, l) -> app (const s) (List.map of_term l)

  let rec of_form f = match f with
    | Form.Bool true -> true_term
    | Form.Bool false -> false_term
    | Form.Nary (Form.And, l) ->
      app (const Symbol.and_symbol) (List.map of_form l)
    | Form.Nary (Form.Or, l) ->
      app (const Symbol.or_symbol) (List.map of_form l)
    | Form.Binary (Form.Equiv, f1, f2) ->
      app (const Symbol.equiv_symbol) [of_form f1; of_form f2]
    | Form.Binary (Form.Imply, f1, f2) ->
      app (const Symbol.imply_symbol) [of_form f1; of_form f2]
    | Form.Equal (t1, t2) ->
      app (const Symbol.eq_symbol) [of_term t1; of_term t2]
    | Form.Not f' -> at (const Symbol.not_symbol) (of_form f')
    | Form.Atom p -> of_term p
    | Form.Quant (Form.Forall, l, f') ->
      forall_list (List.map of_term l) (of_form f')
    | Form.Quant (Form.Exists, l, f') ->
      exists_list (List.map of_term l) (of_form f')

  let rec pp buf t = match t with
    | Const s -> Symbol.pp buf s
    | Var (s, ty) -> Printf.bprintf buf "%s:%a" s Type.Parsed.pp ty
    | App (_, []) -> assert false
    | App (t, l) -> Util.pp_list ~sep:" @ " pp_inner buf (t :: l)
    | Lambda (v, t') ->
      Printf.bprintf buf "λ%a. %a" pp v pp_inner t'

  and pp_inner buf t = match t with
    | App _
    | Lambda _ -> 
      Buffer.add_char buf '(';
      pp buf t;
      Buffer.add_char buf ')'
    | Var _ | Const _ -> pp buf t

  let rec pp_tstp buf t = match t with
    | Const s when Symbol.eq s Symbol.forall_symbol -> Buffer.add_string buf "!!"
    | Const s when Symbol.eq s Symbol.exists_symbol -> Buffer.add_string buf "??"
    | Const s -> Symbol.pp buf s
    | Var (s, ty) -> Printf.bprintf buf "%s:%a" s Type.Parsed.pp ty
    | App (_, []) -> assert false
    | App (t, l) -> Util.pp_list ~sep:" @ " pp_inner buf (t :: l)
    | Lambda (v, t') ->
      Printf.bprintf buf "^[%a]: %a" pp v pp_inner t'

  and pp_inner buf t = match t with
    | App _
    | Lambda _ -> 
      Buffer.add_char buf '(';
      pp_tstp buf t;
      Buffer.add_char buf ')'
    | Var _ | Const _ -> pp buf t

  let to_string = Util.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end
