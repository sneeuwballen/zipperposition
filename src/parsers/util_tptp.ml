
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Utils related to TPTP parsing} *)

open Libzipperposition

module T = TypedSTerm
module PT = STerm
module F = T.Form
module A = Ast_tptp
module Loc = ParseLocation
module Err = CCError

type 'a or_error = [`Error of string | `Ok of 'a]

type typed = T.t
type untyped = PT.t

(** {2 Printing/Parsing} *)

let find_file name dir =
  (* check if the file exists *)
  let file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  in
  (* search in [dir], and its parents recursively *)
  let rec search dir =
    let cur_name = Filename.concat dir name in
    Util.debugf 2 "search `%s`@ as `%s`" (fun k->k name cur_name);
    if file_exists cur_name
    then Some cur_name
    else
      let dir' = Filename.dirname dir in
      if dir = dir'
      then None
      else search dir'
  in
  let search_env () =
    try
      let dir' = Sys.getenv "TPTP" in
      search dir'
    with Not_found -> None
  in
  if Filename.is_relative name
  (* search by relative path, in parent dirs *)
  then match search dir with
    | None -> search_env ()
    | Some _ as res -> res
  else if file_exists name
  then Some name  (* found *)
  else None

(* raise a readable parse error *)
let _raise_error msg lexbuf =
  let loc = Loc.of_lexbuf lexbuf in
  failwith (msg^ Loc.to_string loc)

let parse_lexbuf ?names buf =
  try
    (* parse declarations from file *)
    let decls =
      try Parse_tptp.parse_declarations Lex_tptp.token buf
      with
      | Parse_tptp.Error -> _raise_error "parse error at " buf
      | Failure msg -> _raise_error msg buf
      | Ast_tptp.ParseError loc -> failwith ("parse error at "^Loc.to_string loc)
    in
    let q = Queue.create () in
    List.iter
      (fun decl -> match decl, names with
         | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), None ->
             Queue.push decl q
         | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), Some names ->
             if List.mem (A.get_name decl) names
             then Queue.push decl q
             else ()   (* not included *)
         | (A.Include _ | A.IncludeOnly _), _ ->
             Queue.push decl q)
      decls;
    Err.return (Sequence.of_queue q)
  with
  | Failure msg | Sys_error msg ->
      Err.fail msg
  | e ->
      Err.fail (Printexc.to_string e)

(* find file *)
let _find_and_open filename dir =
  match filename with
  | "stdin" -> stdin
  | _ ->
      match find_file filename dir with
      | Some filename ->
          begin try open_in filename
            with Sys_error msg ->
              failwith ("error when opening file " ^ filename ^ " : " ^ msg)
          end
      | None -> failwith ("could not find file " ^ filename)

let parse_file ~recursive f =
  let dir = Filename.dirname f in
  let result_decls = Queue.create () in
  (* function that parses one file *)
  let rec parse_this_file ?names filename =
    (* find and open file *)
    let input = _find_and_open filename dir in
    try
      let buf = Lexing.from_channel input in
      Loc.set_file buf filename;
      (* parse declarations from file *)
      let decls =
        try Parse_tptp.parse_declarations Lex_tptp.token buf
        with
        | Parse_tptp.Error -> _raise_error "parse error at " buf
        | Ast_tptp.ParseError loc -> failwith ("parse error at "^Loc.to_string loc)
      in
      List.iter
        (fun decl -> match decl, names with
           | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), None ->
               Queue.push decl result_decls
           | (A.CNF _ | A.FOF _ | A.TFF _ | A.THF _ | A.TypeDecl _ | A.NewType _), Some names ->
               if List.mem (A.get_name decl) names
               then Queue.push decl result_decls
               else ()   (* not included *)
           | A.Include f, _ when recursive ->
               parse_this_file ?names:None f
           | A.IncludeOnly (f, names'), _ when recursive ->
               parse_this_file ~names:names' f
           | (A.Include _ | A.IncludeOnly _), _ ->
               Queue.push decl result_decls)
        decls
    with e ->
      close_in input;
      raise e
  in
  try
    parse_this_file ?names:None f;
    Err.return (Sequence.of_queue result_decls)
  with
  | Failure msg | Sys_error msg -> Err.fail msg
  | e -> Err.fail (Printexc.to_string e)

let fpf = Format.fprintf

let print_into ppt oc decls =
  fpf oc "@[<v>%a@]@?"
    (CCFormat.seq ~start:"" ~stop:"" ~sep:"" (A.pp ppt))
    decls

let print_into_file ppt file decls =
  CCIO.with_out file
    (fun oc ->
       let out = Format.formatter_of_out_channel oc in
       print_into ppt out decls;
       Format.pp_print_flush out ();
    )

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

let is_conjecture_ = function
  | A.R_conjecture -> true
  | _ -> false

let formulas ?(negate=is_conjecture_) decls =
  Sequence.filter_map
    (function
      | A.TypeDecl _
      | A.NewType _
      | A.Include _
      | A.IncludeOnly _ -> None
      | A.CNF(_, role, c, _) ->
          if negate role
          then Some (F.not_ (F.or_ c))
          else Some (F.or_ c)
      | A.FOF(_, role, f, _)
      | A.TFF(_, role, f, _) ->
          if negate role
          then Some (F.not_ f)
          else Some f
      | A.THF _ -> None)
    decls

let type_declarations decls =
  (* traverse the declarations, updating the signature when a type decl is met *)
  let a = object
    inherit [typed ID.Map.t, typed] A.visitor
    method! tydecl sigma s ty = ID.Map.add s ty sigma
  end in
  Sequence.fold a#visit ID.Map.empty decls

(* default function for giving a name to the declaration of a symbol *)
let name_sym_ sy =
  let str = CCFormat.sprintf "'ty_decl_%d_%a'" (ID.id sy) ID.pp sy in
  A.NameString str

let declare_symbols_seq ?(name=name_sym_) seq =
  Sequence.map
    (fun (s, ty) ->
       let name = name s in
       A.TypeDecl (name, s, ty, []))
    seq

let declare_symbols ?name sigma =
  let seq = ID.Map.to_seq sigma in
  declare_symbols_seq ?name seq

(** {2 Type inference} *)

let infer_types_exn ?(ctx=TypeInference.Ctx.create ()) decls =
  let module TI = TypeInference in
  let section = TI.section in
  let v =
    Sequence.flat_map
      (fun decl ->
         Util.debugf 3 ~section "@[<2>infer type for@ @[%a@]@]"
           (fun k->k (A.pp PT.pp) decl);
         let d = match decl with
         | A.Include f -> A.Include f
         | A.IncludeOnly (f,l) -> A.IncludeOnly (f,l)
         | A.NewType (n,s,ty,g) ->
             (* convert [ty], then declare it *)
             let ty = TI.infer_ty_exn ctx ty in
             TI.Ctx.declare ctx s ty;
             A.NewType (n,s,ty,g)
         | A.TypeDecl(n, s, ty,g) ->
             let ty = TI.infer_ty_exn ctx ty in
             TI.Ctx.declare ctx s ty;
             A.TypeDecl (n,s,ty,g)
         | A.CNF(n,r,c,i) ->
             let c = TI.infer_clause_exn ctx c in
             TI.Ctx.exit_scope ctx;
             A.CNF(n,r,c,i)
         | A.FOF(n,r,f,i) ->
             let f = TI.infer_prop_exn ctx f in
             let f = T.Form.close_forall f in
             TI.Ctx.exit_scope ctx;
             A.FOF(n,r,f,i)
         | A.TFF(n,r,f,i) ->
             let f = TI.infer_prop_exn ctx f in
             let f = T.Form.close_forall f in
             TI.Ctx.exit_scope ctx;
             A.TFF(n,r,f,i)
         | A.THF(n,r,f,i) ->
             let f = TI.infer_prop_exn ctx f in
             let f = T.Form.close_forall f in
             TI.Ctx.exit_scope ctx;
             A.THF(n,r,f,i)
         in
         let tys =
           TI.Ctx.pop_new_types ctx
           |> Sequence.of_list
           |> declare_symbols_seq ?name:None
         in
         Sequence.append tys (Sequence.return d)
      )
      decls
    |> CCVector.of_seq ?init:None
  in
  (* be sure to traverse the list of declarations *)
  TypeInference.Ctx.bind_to_default ctx;
  CCVector.to_seq v

let infer_types ?ctx seq =
  try Err.return (infer_types_exn ?ctx seq)
  with e -> Err.of_exn_trace e

let erase_types typed =
  Sequence.map
    (function
      | A.Include f -> A.Include f
      | A.IncludeOnly (f,l) -> A.IncludeOnly (f,l)
      | A.NewType (n,s,ty,g) ->
          A.NewType (n,s, T.erase ty, g)
      | A.TypeDecl(n, s, ty, g) ->
          A.TypeDecl (n,s, T.erase ty, g)
      | A.CNF(n,r,c,i) ->
          let c' = List.map T.erase c in
          A.CNF(n,r,c',i)
      | A.FOF(n,r,f,i) ->
          let f' = T.erase f in
          A.FOF(n,r,f',i)
      | A.TFF(n,r,f,i) ->
          let f' = T.erase f in
          A.TFF(n,r,f',i)
      | A.THF(n,r,f,i) ->
          let f' = T.erase f in
          A.THF(n,r,f',i)
    ) typed

let to_cnf ?opts decls =
  (* formulas with correct negation sign *)
  let ctx = Skolem.create () in
  let res = CCVector.create() in
  Sequence.iter
    (function
     | A.TFF(name,r,f,_)
     | A.FOF(name,r,f,_) ->
         let f, role = match r with
           | A.R_conjecture -> F.not_ f, A.R_negated_conjecture
           | _ -> f, r
         in
         let stmts = Cnf.cnf_of ?opts ~ctx f (role, A.string_of_name name) in
         CCVector.append res stmts
     | A.NewType (name, id, ty, _)
     | A.TypeDecl (name, id, ty, _) ->
         CCVector.push res (Cnf.TyDecl (id, ty, (A.R_type, A.string_of_name name)));
     | A.THF _ -> failwith "cnf_of_tptp cannot deal with HO terms right now."
     | _ -> ())
    decls;
  CCVector.freeze res

