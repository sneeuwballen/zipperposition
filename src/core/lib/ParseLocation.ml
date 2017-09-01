
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Location in a file} *)

type t = {
  file : string;
  start_line : int;
  start_column : int;
  stop_line : int;
  stop_column : int;
}

let mk file start_line start_column stop_line stop_column =
  { file; start_line; start_column; stop_line; stop_column; }

let mk_pair file (a,b)(c,d) = mk file a b c d

let mk_pos start stop =
  let open Lexing in
  mk
    start.pos_fname
    start.pos_lnum (start.pos_cnum - start.pos_bol)
    stop.pos_lnum (stop.pos_cnum - stop.pos_bol)

let eq a b = a = b
let hash a = Hashtbl.hash a

let _min_pos (l1,c1) (l2,c2) =
  if l1 = l2
  then l1, min c1 c2
  else if l1 < l2
  then l1, c1
  else l2, c2

let _max_pos (l1,c1) (l2,c2) =
  if l1 = l2
  then l1, max c1 c2
  else if l1 < l2
  then l2, c2
  else l1, c1

let combine p1 p2 =
  let start_line, start_column =
    _min_pos (p1.start_line, p1.start_column) (p2.start_line, p2.start_column)
  in
  let stop_line, stop_column =
    _max_pos (p1.stop_line, p1.stop_column) (p2.stop_line, p2.stop_column)
  in
  { file=p1.file; start_line; start_column; stop_line; stop_column; }

let rec combine_list l = match l with
  | [] -> raise (Invalid_argument "location.combine_list: empty list")
  | [p] -> p
  | p1::((_ ::_) as l') ->
    let p' = combine_list l' in
    combine p1 p'

let smaller p1 p2 =
  (p1.start_line > p2.start_line
   ||  (p1.start_line = p2.start_line && p1.start_column >= p2.start_column))
  &&
  (p1.stop_line < p2.stop_line
   ||  (p1.stop_line = p2.stop_line && p1.stop_column <= p2.stop_column))

module Infix = struct
  let (<+>) = CCOpt.(<+>)
end
include Infix

let pp out pos =
  if pos.start_line = pos.stop_line
  then
    Format.fprintf out "@[file '%s': line %d, col %d to %d@]"
      pos.file pos.start_line pos.start_column pos.stop_column
  else
    Format.fprintf out "@[file '%s': line %d, col %d to line %d, col %d@]"
      pos.file
      pos.start_line pos.start_column
      pos.stop_line pos.stop_column

let to_string = CCFormat.to_string pp

let pp_opt out o = match o with
  | None -> ()
  | Some pos -> Format.fprintf out "@[at %a@]" pp pos

let set_file buf filename =
  let open Lexing in
  buf.lex_curr_p <- {buf.lex_curr_p with pos_fname=filename;};
  ()

let of_lexbuf lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let end_ = Lexing.lexeme_end_p lexbuf in
  let s_l = start.Lexing.pos_lnum in
  let s_c = start.Lexing.pos_cnum - start.Lexing.pos_bol in
  let e_l = end_.Lexing.pos_lnum in
  let e_c = end_.Lexing.pos_cnum - end_.Lexing.pos_bol in
  let file = start.Lexing.pos_fname in
  mk file s_l s_c e_l e_c
