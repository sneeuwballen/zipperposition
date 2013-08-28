
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

(** {2 Printing/Parsing} *)

let find_file name dir =
  (* check if the file exists *)
  let rec file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  (* search in [dir], and its parents recursively *)
  and search dir =
    let cur_name = Filename.concat dir name in
    Util.debug 2 "%% search %s as %s" name cur_name;
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

let parse_file ~recursive f =
  let dir = Filename.dirname f in
  let result_decls = Queue.create () in
  let includes = Queue.create () in
  (* function that parses one file *)
  let rec parse_this_file ?names filename =
    let input = match filename with
    | "stdin" -> stdin
    | _ -> open_in (find_file filename dir) in
    begin try
      let buf = Lexing.from_channel input in
      let decls = Parse_tptp.parse_declarations Lex_tptp.token buf in
      List.iter
        (fun decl -> match decl, names with
          | (A.CNF _ | A.FOF _ | A.TFF _ | A.TypeDecl _), None ->
            Queue.push decl result_decls
          | (A.CNF _ | A.FOF _ | A.TFF _ | A.TypeDecl _), Some names ->
            if List.mem (A.name_of_decl decl) names
              then Queue.push decl result_decls
              else ()   (* not included *)
          | (A.Include _ | A.IncludeOnly _), _ when recursive ->
            Queue.push decl includes
          | (A.Include _ | A.IncludeOnly _), _ ->
            Queue.push decl result_decls)
        decls
    with _ as e ->
      close_in input;
      raise e
    end;
    if recursive && not (Queue.is_empty includes)
      then
        match Queue.pop includes with
        | A.Include filename -> parse_this_file filename
        | A.IncludeOnly (filename, names) -> parse_this_file ~names filename
        | _ -> assert false
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
      | A.TypeDecl _ -> false)
    decls

(** {2 Type inference} *)

let infer_type ctx decls =
  assert false

let signature ?(signature=Signature.empty) decls =
  let ctx = TypeInference.Ctx.of_signature signature in
  infer_type ctx decls;
  TypeInference.Ctx.to_signature ctx
