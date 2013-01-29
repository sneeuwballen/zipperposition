(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* {1 Basic S-expressions, with printing and parsing} *)

(** S-expression *)
type t =
  | Atom of string  (** An atom *)
  | List of t list  (** A list of S-expressions *)

(** Token that compose a Sexpr once serialized *)
type token = [`Open | `Close | `Atom of string]

(** Iterate on the S-expression, calling the callback with tokens *)
let rec iter f s = match s with
  | Atom a -> f (`Atom a)
  | List l -> f `Open; iter_list f l; f `Close
and iter_list f l = match l with
  | [] -> ()
  | x::l' -> iter f x; iter_list f l'

(** Traverse. This yields a sequence of tokens *)
let traverse s = Sequence.from_iter (fun k -> iter k s)

(** Lex: create a sequence of tokens from the given in_channel. *)
let lex input =
  let seq_fun k =
    (* loop. TODO handle escaping of () *)
    let rec next () =
      (match input_char input with
      | '(' -> k `Open
      | ')' -> k `Close
      | ' ' | '\t' | '\n' ->
        if !in_word then begin
          (* this whitespace follows a word *)
          let word = Buffer.contents buf in
          Buffer.clear buf;
          in_word := false;
          k (`Atom word)
        end
      | c -> in_word := true; Buffer.add_char c buf);
      next ()
    in
    try next (); assert false  (* should raise an exception *)
    with End_of_file -> ()
  in { seq_fun; }

(** Build a Sexpr from a sequence of tokens *)
let of_seq seq =
  let stack = ref [] in
  (* called on every token *)
  let k token = match token with
    | `Open -> stack := `Open :: !stack
    | `Close -> stack := close [] !stack
    | `Atom a -> stack := (`Expr (Atom a)) :: !stack
  in
  (* iterate on the sequence *)
  Sequence.iter k seq;
  (* stack should contain exactly one expression *)
  match !stack with
  | [`Expr expr] -> expr
  | [] -> failwith "no Sexpr could be parsed"
  | _ -> failwith "too many elements on the stack"

(** Print a token on the given formatter *)
let pp_token formatter token = match token with
  | `Open -> Format.fprintf formatter "@[("
  | `Close -> Format.fprintf formatter ")@]"
  | `Atom s -> Format.pp_print_string formatter s

(** Print a sequence of Sexpr tokens on the given formatter *)
let pp_tokens formatter tokens =
  Sequence.pp_seq ~sep:" " pp_token formatter tokens

(** Pretty-print the S-expr. If [indent] is true, the S-expression
    is printed with indentation. *)
let ?(indent=false) pp_sexpr formatter s =
  if indent
    then Format.fprintf formatter "@[<hov 4>%a@]" pp_tokens (traverse s)
    else pp_tokens formatter (traverse s)
