
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

(** {1 Utils related to TPTP parsing} *)

module A = Ast_tptp
module T = FOTerm
module F = FOFormula

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
      let decls =
        try Parse_tptp.parse_declarations Lex_tptp.token buf
        with Parse_tptp.Error ->
          _raise_error filename buf  (* report error properly *)
      in
      List.iter
        (fun decl -> match decl, names with
          | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), None ->
            Queue.push decl result_decls
          | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), Some names ->
            if List.mem (A.name_of_decl decl) names
              then Queue.push decl result_decls
              else ()   (* not included *)
          | A.Include f, _ when recursive ->
            parse_this_file ?names:None f
          | A.IncludeOnly (f, names'), _ when recursive ->
            parse_this_file ~names:names' f
          | (A.Include _ | A.IncludeOnly _), _ ->
            Queue.push decl result_decls)
        decls
    with _ as e ->
      close_in input;
      raise e
    end;
  in
  parse_this_file ?names:None f;
  Sequence.of_queue result_decls

let print_into oc decls =
  Sequence.iter
    (fun decl -> Util.fprintf oc "%a\n" A.pp_declaration decl)
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
    (fun decl -> Printf.bprintf buf "%a\n" A.pp_declaration decl)
    decls

let has_includes decls =
  Sequence.exists
    (function
      | A.Include _
      | A.IncludeOnly _ -> true
      | A.FOF _
      | A.CNF _
      | A.TFF _
      | A.THF _
      | A.NewType _
      | A.TypeDecl _ -> false)
    decls

(** {2 Type inference} *)

let infer_type ctx decls =
  Sequence.iter
    (fun decl ->
      Util.debug 3 "infer type for %a" A.pp_declaration decl;
      match decl with
      | A.Include _
      | A.IncludeOnly _ -> ()
      | A.NewType _ -> ()  (* ignore *)
      | A.TypeDecl(_, s, ty) ->
        TypeInference.Ctx.declare ctx s ty
      | A.CNF(_,_,c,_) ->
        List.iter (F.infer_type ctx) c
      | A.FOF(_,_,f,_)
      | A.TFF(_,_,f,_) -> F.infer_type ctx f
      | A.THF(_,_,f,_) -> ignore (TypeInference.HO.infer ctx f)
      )
    decls

let signature ?(signature=Signature.base) decls =
  let ctx = TypeInference.Ctx.of_signature signature in
  infer_type ctx decls;
  TypeInference.Ctx.to_signature ctx

let type_declarations decls =
  Sequence.fold
    (fun signature decl -> match decl with
      | A.TypeDecl (_, s, ty) -> Signature.declare signature s ty
      | _ -> signature)
    Signature.empty decls

let __name_symbol i sy =
  let str = Util.sprintf "'ty_decl_%d_%a'" i Symbol.pp sy in
  A.NameString str

let declare_symbols ?(name=__name_symbol) signature =
  let signature = Signature.diff signature Signature.base in
  let seq = Signature.to_seq signature in
  Sequence.mapi
    (fun i (s, ty) ->
      let name = name i s in
      A.TypeDecl (name, s, ty))
    seq

let __is_conjecture = function
  | A.R_conjecture -> true
  | _ -> false

let formulas ?(negate=__is_conjecture) decls =
  Sequence.fmap
    (function
      | A.TypeDecl _
      | A.NewType _
      | A.Include _
      | A.IncludeOnly _ -> None
      | A.CNF(_, role, c, _) ->
        if negate role
          then Some (F.mk_not (F.mk_or c))
          else Some (F.mk_or c)
      | A.FOF(_, role, f, _)
      | A.TFF(_, role, f, _) ->
        if negate role
          then Some (F.mk_not f)
          else Some f
      | A.THF _ -> None)
    decls

let sourced_formulas ?(negate=__is_conjecture) ?(file="unknown_file") decls =
  Sequence.fmap
    (function
      | A.TypeDecl _
      | A.NewType _
      | A.Include _
      | A.IncludeOnly _ -> None
      | A.CNF(name, role, c, _) ->
        let source = A.string_of_name name in
        if negate role
          then Some (F.mk_not (F.mk_or c), file, source)
          else Some (F.mk_or c, file, source)
      | A.FOF(name, role, f, _)
      | A.TFF(name, role, f, _) ->
        let source = A.string_of_name name in
        if negate role
          then Some (F.mk_not f, file, source)
          else Some (f, file, source)
      | A.THF _ -> None)
    decls
