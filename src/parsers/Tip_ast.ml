
(* This file is free software. See file "license" for more details. *)

(** {1 Trivial AST for parsing} *)

open Logtk

let pp_str = Format.pp_print_string

let pp_to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

module Loc = ParseLocation

type var = string
type ty_var = string

(** Polymorphic types *)
type ty =
  | Ty_bool
  | Ty_app of ty_var * ty list
  | Ty_arrow of ty list * ty

type typed_var = var * ty

(** {2 AST: S-expressions with locations} *)
type term =
  | True
  | False
  | Const of string
  | App of string * term list
  | HO_app of term * term (* higher-order application *)
  | Match of term * match_branch list
  | If of term * term * term
  | Let of (var * term) list * term
  | Fun of typed_var * term
  | Eq of term * term
  | Imply of term * term
  | And of term list
  | Or of term list
  | Not of term
  | Distinct of term list
  | Cast of term * ty (* type cast *)
  | Forall of (var * ty) list * term
  | Exists of (var * ty) list * term

and match_branch =
  | Match_default of term
  | Match_case of string * var list * term

type cstor = {
  cstor_name: string;
  cstor_args: (string * ty) list; (* selector+type *)
}

type 'arg fun_decl = {
  fun_ty_vars: ty_var list;
  fun_name: string;
  fun_args: 'arg list;
  fun_ret: ty;
}

type fun_def = {
  fr_decl: typed_var fun_decl;
  fr_body: term;
}

type funs_rec_def = {
  fsr_decls: typed_var fun_decl list;
  fsr_bodies: term list;
}

type statement = {
  stmt: stmt;
  loc: Loc.t option;
}

and stmt =
  | Stmt_decl_sort of string * int (* arity *)
  | Stmt_decl of ty fun_decl
  | Stmt_fun_def of fun_def
  | Stmt_fun_rec of fun_def
  | Stmt_funs_rec of funs_rec_def
  | Stmt_data of ty_var list * (string * cstor list) list
  | Stmt_assert of term
  | Stmt_lemma of term
  | Stmt_assert_not of ty_var list * term
  | Stmt_check_sat

let ty_bool = Ty_bool
let ty_app s l = Ty_app (s,l)
let ty_const s = ty_app s []
let ty_arrow_l args ret = if args=[] then ret else Ty_arrow (args, ret)
let ty_arrow a b = ty_arrow_l [a] b

let true_ = True
let false_ = False
let const s = Const s
let app f l = App (f,l)
let ho_app a b = HO_app (a,b)
let ho_app_l a l = List.fold_left ho_app a l
let match_ u l = Match (u,l)
let if_ a b c = If(a,b,c)
let fun_ v t = Fun (v,t)
let fun_l = List.fold_right fun_
let let_ l t = Let (l,t)
let eq a b = Eq (a,b)
let imply a b = Imply(a,b)
let and_ l = And l
let or_ l = Or l
let distinct l = Distinct l
let cast t ~ty = Cast (t, ty)
let forall vars f = match vars with [] -> f | _ -> Forall (vars, f)
let exists vars f = match vars with [] -> f | _ -> Exists (vars, f)
let rec not_ t = match t with
  | Forall (vars,u) -> exists vars (not_ u)
  | Exists (vars,u) -> forall vars (not_ u)
  | _ -> Not t

let _mk ?loc stmt = { loc; stmt }

let mk_cstor name l : cstor = { cstor_name=name; cstor_args=l }
let mk_fun_decl ~ty_vars f args ret =
  { fun_ty_vars=ty_vars; fun_name=f;
    fun_args=args; fun_ret=ret; }
let mk_fun_rec ~ty_vars f args ret body =
  { fr_decl=mk_fun_decl ~ty_vars f args ret; fr_body=body; }

let decl_sort ?loc s ~arity = _mk ?loc (Stmt_decl_sort (s, arity))
let decl_fun ?loc ~tyvars f ty_args ty_ret =
  let d = {fun_ty_vars=tyvars; fun_name=f; fun_args=ty_args; fun_ret=ty_ret} in
  _mk ?loc (Stmt_decl d)
let fun_def ?loc fr = _mk ?loc (Stmt_fun_def fr)
let fun_rec ?loc fr = _mk ?loc (Stmt_fun_rec fr)
let funs_rec ?loc decls bodies = _mk ?loc (Stmt_funs_rec {fsr_decls=decls; fsr_bodies=bodies})
let data ?loc tyvars l = _mk ?loc (Stmt_data (tyvars,l))
let assert_ ?loc t = _mk ?loc (Stmt_assert t)
let lemma ?loc t = _mk ?loc (Stmt_lemma t)
let assert_not ?loc ~ty_vars t = _mk ?loc (Stmt_assert_not (ty_vars, t))
let check_sat ?loc () = _mk ?loc Stmt_check_sat

let loc t = t.loc
let view t = t.stmt

let fpf = Format.fprintf

let pp_list ?(start="") ?(stop="") ?(sep=" ") pp out l =
  let rec pp_list l = match l with
    | x::((_::_) as l) ->
      pp out x;
      Format.pp_print_string out sep;
      Format.pp_print_cut out ();
      pp_list l
    | x::[] -> pp out x
    | [] -> ()
  in
  Format.pp_print_string out start;
  pp_list l;
  Format.pp_print_string out stop


let pp_tyvar = pp_str

let rec pp_ty out (ty:ty) = match ty with
  | Ty_bool -> pp_str out "Bool"
  | Ty_app (s,[]) -> pp_str out s
  | Ty_app (s,l) -> Format.fprintf out "(@[<hv1>%s@ %a@])" s (pp_list pp_ty) l
  | Ty_arrow (args,ret) ->
    fpf out "(@[=>@ %a@ %a@])" (pp_list pp_ty) args pp_ty ret

let rec pp_term out (t:term) = match t with
  | True -> pp_str out "true"
  | False -> pp_str out "false"
  | Const s -> pp_str out s
  | App (f,l) -> fpf out "(@[<1>%s@ %a@])" f (pp_list pp_term) l
  | HO_app (a,b) -> fpf out "(@[<1>@@@ %a@ %a@])" pp_term a pp_term b
  | Match (lhs,cases) ->
    let pp_case out = function
      | Match_default rhs -> fpf out "(@[<2>case default@ %a@])" pp_term rhs
      | Match_case (c,[],rhs) ->
        fpf out "(@[<2>case %s@ %a@])" c pp_term rhs
      | Match_case (c,vars,rhs) ->
        fpf out "(@[<2>case@ (@[%s@ %a@])@ %a@])" c (pp_list pp_str) vars pp_term rhs
    in
    fpf out "(@[<1>match@ %a@ @[<v>%a@]@])" pp_term lhs
      (pp_list pp_case) cases
  | If (a,b,c) -> fpf out "(@[<hv1>ite %a@ %a@ %a@])" pp_term a pp_term b pp_term c
  | Fun (v,body) -> fpf out "(@[<1>lambda @ (%a)@ %a@])" pp_typed_var v pp_term body
  | Let (l,t) ->
    let pp_binding out (v,t) = fpf out "(@[%s@ %a@])" v pp_term t in
    fpf out "(@[<2>let@ (@[%a@])@ %a@])" (pp_list pp_binding) l pp_term t
  | Eq (a,b) -> fpf out "(@[=@ %a@ %a@])" pp_term a pp_term b
  | Imply (a,b) -> fpf out "(@[=>@ %a@ %a@])" pp_term a pp_term b
  | And l -> fpf out "(@[<hv>and@ %a@])" (pp_list pp_term) l
  | Or l -> fpf out "(@[<hv>or@ %a@])" (pp_list pp_term) l
  | Not t -> fpf out "(not %a)" pp_term t
  | Distinct l -> fpf out "(@[distinct@ %a@])" (pp_list pp_term) l
  | Cast (t, ty) -> fpf out "(@[<hv2>as@ @[%a@]@ @[%a@]@])" pp_term t pp_ty ty
  | Forall (vars,f) ->
    fpf out "(@[<hv2>forall@ (@[%a@])@ %a@])" (pp_list pp_typed_var) vars pp_term f
  | Exists (vars,f) ->
    fpf out "(@[<hv2>exists@ (@[%a@])@ %a@])" (pp_list pp_typed_var) vars pp_term f
and pp_typed_var out (v,ty) =
  fpf out "(@[%s@ %a@])" v pp_ty ty

let pp_par pp_x out (ty_vars,x) = match ty_vars with
  | [] -> pp_x out x
  | _ ->
    fpf out "(@[<2>par (@[%a@])@ (%a)@])" (pp_list pp_tyvar) ty_vars pp_x x

let pp_fun_decl pp_arg out fd =
  fpf out "%s@ (@[%a@])@ %a"
    fd.fun_name (pp_list pp_arg) fd.fun_args pp_ty fd.fun_ret

let pp_fr out fr =
  fpf out "@[<2>%a@ %a@]" (pp_fun_decl pp_typed_var) fr.fr_decl pp_term fr.fr_body

let pp_stmt out (st:statement) = match view st with
  | Stmt_decl_sort (s,n) -> fpf out "(@[declare-sort@ %s %d@])" s n
  | Stmt_assert t -> fpf out "(@[assert@ %a@])" pp_term t
  | Stmt_lemma t -> fpf out "(@[lemma@ %a@])" pp_term t
  | Stmt_assert_not (ty_vars,t) ->
    fpf out "(@[assert-not@ %a@])" (pp_par pp_term) (ty_vars,t)
  | Stmt_decl d ->
    fpf out "(@[declare-fun@ %a@])"
      (pp_par (pp_fun_decl pp_ty)) (d.fun_ty_vars,d)
  | Stmt_fun_def fr ->
    fpf out "(@[<2>define-fun@ %a@])"
      (pp_par pp_fr) (fr.fr_decl.fun_ty_vars, fr)
  | Stmt_fun_rec fr ->
    fpf out "(@[<2>define-fun-rec@ %a@])"
      (pp_par pp_fr) (fr.fr_decl.fun_ty_vars, fr)
  | Stmt_funs_rec fsr ->
    let pp_decl' out d = fpf out "(@[<2>%a@])" (pp_fun_decl pp_typed_var) d in
    fpf out "(@[<hv2>define-funs-rec@ (@[<v>%a@])@ (@[<v>%a@])@])"
      (pp_list pp_decl') fsr.fsr_decls (pp_list pp_term) fsr.fsr_bodies
  | Stmt_data (tyvars,l) ->
    let pp_cstor_arg out (sel,ty) = fpf out "(@[%s %a@])" sel pp_ty ty in
    let pp_cstor out c =
      if c.cstor_args = []
      then fpf out "(%s)" c.cstor_name
      else fpf out "(@[<1>%s@ %a@])" c.cstor_name (pp_list pp_cstor_arg) c.cstor_args
    in
    let pp_data out (s,cstors) =
      fpf out "(@[<2>%s@ @[<v>%a@]@])" s (pp_list pp_cstor) cstors
    in
    fpf out "(@[<hv2>declare-datatypes@ (@[%a@])@ (@[<v>%a@])@])"
      (pp_list pp_tyvar) tyvars (pp_list pp_data) l
  | Stmt_check_sat -> pp_str out "(check-sat)"

(** {2 Errors} *)

exception Parse_error of Loc.t option * string

let () = Printexc.register_printer
    (function
      | Parse_error (loc, msg) ->
        let pp out () =
          Format.fprintf out "parse error at %a:@ %s" Loc.pp_opt loc msg
        in
        Some (pp_to_string pp ())
      | _ -> None)

let parse_error ?loc msg = raise (Parse_error (loc, msg))
let parse_errorf ?loc msg = Format.ksprintf (parse_error ?loc) msg
