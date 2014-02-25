
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBBTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BBT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBBTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BBT NOT LIMITED TO, PROCUREMENT OF SUBSTITBTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OBT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Utils related to TPTP parsing} *)

open Logtk

module T = FOTerm
module F = Formula.FO
module PT = PrologTerm
module Ast = Ast_tptp
module AU = Ast.Untyped
module AT = Ast.Typed
module Loc = ParseLocation

(** {2 Printing/Parsing} *)

let find_file name dir =
  (* check if the file exists *)
  let rec file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  (* search in [dir], and its parents recursively *)
  and search dir =
    let cur_name = Filename.concat dir name in
    Util.debug 2 "search %s as %s" name cur_name;
    if file_exists cur_name then cur_name
    else
      let dir' = Filename.dirname dir in
      if dir = dir'
        then failwith "unable to find file"
        else search dir'
  in
  if Filename.is_relative name
    then try
      search dir  (* search by relative path, in parent dirs *)
    with (Failure _) as e ->
      (try let dir' = Sys.getenv "TPTP" in
        search dir'
      with Not_found -> raise e)
    else if file_exists name
      then name  (* found *)
      else failwith ("unable to find file " ^ name)

exception ParseError of string * int * int * int * int
  (** Error at the given file, start(line, column) stop(line,column) *)

let string_of_error = function
  | ParseError (filename, s_l, s_c, e_l, e_c) ->
    Printf.sprintf "syntax error in %s between %d:%d and %d:%d"
      filename s_l s_c e_l e_c
  | _ -> assert false

(* raise a readable parse error *)
let _raise_error filename lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let end_ = Lexing.lexeme_end_p lexbuf in
  let s_l = start.Lexing.pos_lnum in
  let s_c = start.Lexing.pos_cnum - start.Lexing.pos_bol in
  let e_l = end_.Lexing.pos_lnum in
  let e_c = end_.Lexing.pos_cnum - end_.Lexing.pos_bol in
  raise (ParseError (filename, s_l, s_c, e_l, e_c))

let parse_file ~recursive f =
  let dir = Filename.dirname f in
  let result_decls = Queue.create () in
  (* function that parses one file *)
  let rec parse_this_file ?names filename =
    let input = match filename with
    | "stdin" -> stdin
    | _ -> open_in (find_file filename dir) in
    begin try
      let buf = Lexing.from_channel input in
      Loc.set_file buf filename;
      let decls =
        try Parse_tptp.parse_declarations Lex_tptp.token buf
        with Parse_tptp.Error ->
          _raise_error filename buf  (* report error properly *)
      in
      List.iter
        (fun decl -> match decl, names with
          | (AU.CNF _ | AU.FOF _ | AU.TFF _ | AU.THF _ | AU.TypeDecl _ | AU.NewType _), None ->
            Queue.push decl result_decls
          | (AU.CNF _ | AU.FOF _ | AU.TFF _ | AU.THF _ | AU.TypeDecl _ | AU.NewType _), Some names ->
            if List.mem (AU.get_name decl) names
              then Queue.push decl result_decls
              else ()   (* not included *)
          | AU.Include f, _ when recursive ->
            parse_this_file ?names:None f
          | AU.IncludeOnly (f, names'), _ when recursive ->
            parse_this_file ~names:names' f
          | (AU.Include _ | AU.IncludeOnly _), _ ->
            Queue.push decl result_decls)
        decls
    with (Ast.ParseError loc) as e ->
      close_in input;
      Util.debug 1 "parse_tptp: syntax error: %a" Loc.pp loc;
      raise e
    | _ as e ->
      close_in input;
      raise e
    end;
  in
  parse_this_file ?names:None f;
  Sequence.of_queue result_decls

module type S = sig
  module A : Ast_tptp.S

  val print_into : out_channel -> A.t Sequence.t -> unit
  val print_into_file : string -> A.t Sequence.t -> unit
  val print_into_buf : Buffer.t -> A.t Sequence.t -> unit

  val has_includes : A.t Sequence.t -> bool
    (** Check whether some include declaration can be found in the sequence *)

  val type_declarations : A.t Sequence.t -> Signature.t
    (** Initial signature obtained by only considering the type declarations.
        In contrast to {!signature}, this doesn't perform type inference. *)

  val declare_symbols : ?name:(int -> string -> Ast_tptp.name) ->
                        Signature.t -> A.t Sequence.t
    (** Declare the symbols of the signature. A custom function
        to name the [i]-th symbol declaration can be provided. *)

  val formulas : ?negate:(Ast_tptp.role -> bool) ->
                 A.t Sequence.t -> A.form Sequence.t
    (** Extract only the formulas from some declarations. Formulas with
        a role that satisfies [negate] are negated.
        [negate] is true, by default, only for {!Ast_tptp.R_conjecture})*)

  val sourced_formulas : ?negate:(Ast_tptp.role -> bool) ->
                         ?file:string ->
                         A.t Sequence.t ->
                         (A.form * string * string) Sequence.t
    (** Same as {!formulas}, but keeps a source attached to formulas.
        A [file] name has to be provided for the source to be accurate,
        the default is "unknown_file". *)
end

module Untyped = struct
  module A = Ast_tptp.Untyped
  let print_into oc decls =
    Sequence.iter
      (fun decl -> Util.fprintf oc "%a\n" AU.pp decl)
      decls;
    flush oc

  let print_into_file filename decls =
    let oc = open_out filename in
    try
      print_into oc decls;
      close_out oc
    with e ->
      close_out oc;
      raise e

  let print_into_buf buf decls =
    Sequence.iter
      (fun decl -> Printf.bprintf buf "%a\n" AU.pp decl)
      decls

  let has_includes decls =
    Sequence.exists
      (function
        | AU.Include _
        | AU.IncludeOnly _ -> true
        | AU.FOF _
        | AU.CNF _
        | AU.TFF _
        | AU.THF _
        | AU.NewType _
        | AU.TypeDecl _ -> false)
      decls

  let __is_conjecture = function
    | Ast.R_conjecture -> true
    | _ -> false

  let formulas ?(negate=__is_conjecture) decls =
    Sequence.fmap
      (function
        | AU.TypeDecl _
        | AU.NewType _
        | AU.Include _
        | AU.IncludeOnly _ -> None
        | AU.CNF(_, role, c, _) ->
          if negate role
            then Some (PT.TPTP.not_ (PT.TPTP.or_ c))
            else Some (PT.TPTP.or_ c)
        | AU.FOF(_, role, f, _)
        | AU.TFF(_, role, f, _) ->
          if negate role
            then Some (PT.TPTP.not_ f)
            else Some f
        | AU.THF _ -> None)
      decls

  let sourced_formulas ?(negate=__is_conjecture) ?(file="unknown_file") decls =
    Sequence.fmap
      (function
        | AU.TypeDecl _
        | AU.NewType _
        | AU.Include _
        | AU.IncludeOnly _ -> None
        | AU.CNF(name, role, c, _) ->
          let source = Ast.string_of_name name in
          if negate role
            then Some (PT.TPTP.not_ (PT.TPTP.or_ c), file, source)
            else Some (PT.TPTP.or_ c, file, source)
        | AU.FOF(name, role, f, _)
        | AU.TFF(name, role, f, _) ->
          let source = Ast.string_of_name name in
          if negate role
            then Some (PT.TPTP.not_ f, file, source)
            else Some (f, file, source)
        | AU.THF _ -> None)
      decls

  let type_declarations decls =
    let tyctx = Hashtbl.create 5 in
    let a = object
      inherit [Signature.t] AU.visitor
      method tydecl signature s ty =
        match Type.Conv.of_prolog ~ctx:tyctx ty with
        | None -> raise (Type.Error ("expected type, got " ^ PT.to_string ty))
        | Some ty' -> Signature.declare signature (Symbol.of_string s) ty'
    end in
    Sequence.fold a#visit Signature.empty decls

  let __name_symbol i sy =
    let str = Util.sprintf "'ty_decl_%d_%s'" i sy in
    Ast.NameString str

  let declare_symbols ?(name=__name_symbol) signature =
    let signature = Signature.diff signature Signature.TPTP.base in
    let seq = Signature.Seq.to_seq signature in
    Sequence.mapi
      (fun i (sym, ty) ->
        let s = Symbol.to_string sym in
        let name = name i s in
        AU.TypeDecl (name, s, Type.Conv.to_prolog ty))
      seq
end

module Typed = struct
  module A = Ast_tptp.Typed

  let print_into oc decls =
    Sequence.iter
      (fun decl -> Util.fprintf oc "%a\n" AT.pp decl)
      decls;
    flush oc

  let print_into_file filename decls =
    let oc = open_out filename in
    try
      print_into oc decls;
      close_out oc
    with e ->
      close_out oc;
      raise e

  let print_into_buf buf decls =
    Sequence.iter
      (fun decl -> Printf.bprintf buf "%a\n" AT.pp decl)
      decls

  let has_includes decls =
    Sequence.exists
      (function
        | AT.Include _
        | AT.IncludeOnly _ -> true
        | AT.FOF _
        | AT.CNF _
        | AT.TFF _
        | AT.THF _
        | AT.NewType _
        | AT.TypeDecl _ -> false)
      decls

  let __is_conjecture = function
    | Ast.R_conjecture -> true
    | _ -> false

  let formulas ?(negate=__is_conjecture) decls =
    Sequence.fmap
      (function
        | AT.TypeDecl _
        | AT.NewType _
        | AT.Include _
        | AT.IncludeOnly _ -> None
        | AT.CNF(_, role, c, _) ->
          if negate role
            then Some (F.Base.not_ (F.Base.or_ c))
            else Some (F.Base.or_ c)
        | AT.FOF(_, role, f, _)
        | AT.TFF(_, role, f, _) ->
          if negate role
            then Some (F.Base.not_ f)
            else Some f
        | AT.THF _ -> None)
      decls

  let sourced_formulas ?(negate=__is_conjecture) ?(file="unknown_file") decls =
    Sequence.fmap
      (function
        | AT.TypeDecl _
        | AT.NewType _
        | AT.Include _
        | AT.IncludeOnly _ -> None
        | AT.CNF(name, role, c, _) ->
          let source = Ast.string_of_name name in
          if negate role
            then Some (F.Base.not_ (F.Base.or_ c), file, source)
            else Some (F.Base.or_ c, file, source)
        | AT.FOF(name, role, f, _)
        | AT.TFF(name, role, f, _) ->
          let source = Ast.string_of_name name in
          if negate role
            then Some (F.Base.not_ f, file, source)
            else Some (f, file, source)
        | AT.THF _ -> None)
      decls

  let type_declarations decls =
    let a = object
      inherit [Signature.t] AT.visitor
      method tydecl signature s ty =
        Signature.declare signature (Symbol.of_string s) ty
    end in
    Sequence.fold a#visit Signature.empty decls

  let __name_symbol i sy =
    let str = Util.sprintf "'ty_decl_%d_%s'" i sy in
    Ast.NameString str

  let declare_symbols ?(name=__name_symbol) signature =
    let signature = Signature.diff signature Signature.TPTP.base in
    let seq = Signature.Seq.to_seq signature in
    Sequence.mapi
      (fun i (sym, ty) ->
        let s = Symbol.to_string sym in
        let name = name i s in
        AT.TypeDecl (name, s, ty))
      seq
end

(** {2 Type inference} *)

let infer_types init decls =
  let module TI = TypeInference in
  let ctx = match init with
    | `ctx ctx -> ctx
    | `sign signature -> TI.Ctx.create signature
  in
  let s = Sequence.map
    (fun decl ->
      Util.debug 3 "infer type for %a" AU.pp decl;
      match decl with
      | AU.Include f -> AT.Include f
      | AU.IncludeOnly (f,l) -> AT.IncludeOnly (f,l)
      | AU.NewType (n,s,ty) ->
          begin match TI.Ctx.ty_of_prolog ctx ty with
          | None -> raise (Type.Error ("expected type, got " ^ PT.to_string ty))
          | Some ty' -> AT.NewType (n,s, ty')
          end
      | AU.TypeDecl(n, s, ty) ->
          begin match TI.Ctx.ty_of_prolog ctx ty with
          | None -> raise (Type.Error ("expected type, got " ^ PT.to_string ty))
          | Some ty' ->
              TI.Ctx.declare ctx (Symbol.of_string s) ty';
              AT.TypeDecl (n,s, ty')
          end
      | AU.CNF(n,r,c,i) ->
          let c' = TI.FO.convert_clause ~ctx c in
          AT.CNF(n,r,c',i)
      | AU.FOF(n,r,f,i) ->
          let f' = TI.FO.convert_form ~ctx f in
          AT.FOF(n,r,f',i)
      | AU.TFF(n,r,f,i) ->
          let f' = TI.FO.convert_form ~ctx f in
          AT.TFF(n,r,f',i)
      | AU.THF(n,r,f,i) ->
          failwith "type conversion for HO Term: not implemented" (* TODO *)
      )
    decls
  in
  (* be sure to traverse the list of declarations *)
  let s = Sequence.persistent s in
  TypeInference.Ctx.bind_to_default ctx;
  TI.Ctx.to_signature ctx, s

let signature init decls =
  let ctx = match init with
    | `ctx ctx -> ctx
    | `sign signature -> TypeInference.Ctx.create signature
  in
  let a = object
    inherit [unit] AU.visitor
    method tydecl () s ty =
      begin match TypeInference.Ctx.ty_of_prolog ctx ty with
      | None -> raise (Type.Error ("expected type, got " ^ PT.to_string ty))
      | Some ty' -> 
        TypeInference.Ctx.declare ctx (Symbol.of_string s) ty'
      end
    method any_form () _ f =
      TypeInference.FO.constrain_form ctx f
    method clause () _ c =
      List.iter (TypeInference.FO.constrain_form ctx) c
  end in
  Sequence.fold a#visit () decls;
  TypeInference.Ctx.to_signature ctx

let erase_types typed =
  Sequence.map
    (function
      | AT.Include f -> AU.Include f
      | AT.IncludeOnly (f,l) -> AU.IncludeOnly (f,l)
      | AT.NewType (n,s,ty) ->
          AU.NewType (n,s, Type.Conv.to_prolog ty)
      | AT.TypeDecl(n, s, ty) ->
          AU.TypeDecl (n,s, Type.Conv.to_prolog ty)
      | AT.CNF(n,r,c,i) ->
          let c' = List.map F.to_prolog c in
          AU.CNF(n,r,c',i)
      | AT.FOF(n,r,f,i) ->
          let f' = F.to_prolog f in
          AU.FOF(n,r,f',i)
      | AT.TFF(n,r,f,i) ->
          let f' = F.to_prolog f in
          AU.TFF(n,r,f',i)
      | AT.THF(n,r,f,i) ->
          failwith "type conversion for HO Term: not implemented" (* TODO *)
    ) typed

let annotate_types init untyped =
  let _, typed = infer_types init untyped in
  erase_types typed

let to_cnf signature decls =
  (* formulas with correct negation sign *)
  let ctx = Skolem.create signature in
  let clauses = Sequence.flatMap
    (fun decl -> match decl with
      | AT.TFF(n,r,f,info)
      | AT.FOF(n,r,f,info) ->
        let f, role = match r with
        | Ast.R_conjecture -> F.Base.not_ f, Ast.R_negated_conjecture
        | _ -> f, r
        in
        let clauses = Cnf.cnf_of ~ctx f in
        Sequence.map
          (fun c ->
            match decl with
            | AT.TFF _ -> AT.TFF(n,role, F.close_forall (F.Base.or_ c),info)
            | AT.FOF _ -> AT.CNF(n,role, c,info)
            | _ -> assert false)
            (Sequence.of_list clauses)
      | AT.THF _ -> failwith "cnf_of_tptp cannot deal with HO terms right now."
      | _ -> Sequence.singleton decl
    ) decls
  in
  (* make sure the clauses don't change.
    iterating again would change skolems, etc, which is bad. *)
  let clauses = Sequence.persistent clauses in
  let signature = Skolem.to_signature ctx in
  signature, clauses

