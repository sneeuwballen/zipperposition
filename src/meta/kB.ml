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

(** {2 Persistent Knowledge Base} *)

open Types

module T = Terms
module Utils = FoUtils

(** {3 Knowledge Item} *)

(** Assertions at the meta-level, that respectively state that
    a lemma is true, that define a theory, or that bind a ground
    convergent system to a theory *)
type item =
| Lemma of Pattern.t parametrized * Pattern.t parametrized list
  (** A lemma is the implication of a pattern by other patterns,
      but with some variable renamings to correlate the
      bindings of the distinct patterns. For instance,
      (F(x,y)=x, [F], [Mult]) may be implied by
      (F(y,x)=y, [F], [MyMult]) and
      (F(x,y)=G(y,x), [F,G], [Mult,MyMult]). *)
| Theory of string parametrized * Pattern.t parametrized list
  (** A theory, like a lemma, needs to correlate the variables
      in several patterns via renaming. It outputs an assertion
      about the theory being present for some symbols. *)
| GC of gnd_convergent_spec parametrized * string parametrized
and gnd_convergent_spec = {
  gc_ord : string;
  gc_prec : varlist;
  gc_eqns : Pattern.t list;
} (** Abstract equations that form a ground convergent rewriting system
      when instantiated. It is parametrized by the theory it decides.
      gc_ord and gc_prec (once instantiated), give a constraint on the ordering
      that must be satisfied for the system to be a decision procedure. *)

(** {3 Knowledge Base} *)

type t = item list
  (** The knowledge base. *)

let empty = []

let add_item kb i = i :: kb

let to_seq kb = Sequence.of_list kb

let of_seq kb items =
  Sequence.fold add_item kb items

(** {2 Printing/parsing} *)

(* TODO *)
let pp_item formatter item =
  let open Format in
  match item with
  | Lemma (concl, premises) ->
    fprintf formatter "@[<hov2>%a(%a) if@ %a@]" Pattern.pp_pattern (fst concl)
      (Utils.pp_list !T.pp_term#pp) (snd concl)
      (Utils.pp_list
        (fun formatter (pat,vars) ->
          fprintf formatter "%a(%a)" Pattern.pp_pattern pat
            (Utils.pp_list !T.pp_term#pp) vars))
        premises
  | Theory ((name,args), premises) ->
    fprintf formatter "@[<hov2>theory %s(%a) is@ %a@]" name
      (Utils.pp_list !T.pp_term#pp) args
      (Utils.pp_list
        (fun formatter (pat,vars) ->
          fprintf formatter "%a(%a)" Pattern.pp_pattern pat
            (Utils.pp_list !T.pp_term#pp) vars))
      premises
  | GC ((gc,gc_vars),(th,th_vars)) ->
    fprintf formatter
      "@[<hov2>gc [%a]@ @[<h>when ord %s and prec %a@]@ if @[<h>%s(%a)@]@]"
      (Utils.pp_list Pattern.pp_pattern) gc.gc_eqns
      gc.gc_ord (Utils.pp_list !T.pp_term#pp) gc.gc_prec
      th (Utils.pp_list !T.pp_term#pp) th_vars

let pp formatter kb =
  Utils.pp_list pp_item formatter kb 

let item_to_json item : json =
  let pp_inst_pattern (pat, args) =
    `List (Pattern.to_json pat :: List.map T.to_json args)
  in
  match item with
  | Lemma (concl,premises) ->
    `Assoc ["conclusion", pp_inst_pattern concl;
            "premises", `List (List.map pp_inst_pattern premises);]
  | Theory ((th, th_args), premises) ->
    `Assoc ["theory", `List (`String th :: List.map T.to_json th_args);
            "premises", `List (List.map pp_inst_pattern premises);]
  | GC (gc,th) -> failwith "todo: item_to_json"

let item_of_json (json : json) =
  let inst_pattern_of = function
  | `List (pat::args) -> (Pattern.of_json pat, List.map T.of_json args)
  | json -> raise (Json.Util.Type_error ("expected pattern", json))
  in
  match json with
  | `Assoc ["conclusion", concl; "premises", `List premises] ->
    Lemma (inst_pattern_of concl, List.map inst_pattern_of premises)
  | `Assoc ["theory", `List (`String th :: args); "premises", `List premises] ->
    Theory ((th, List.map T.of_json args), List.map inst_pattern_of premises)
  | _ -> failwith "todo: item_of_json"  (* TODO GndConvergent *)

let to_json kb : json = `List (List.map item_to_json kb)

let of_json kb (json : json) : t =
  let l = Json.Util.to_list json in
  of_seq kb (Sequence.map item_of_json (Sequence.of_list l))

(** {2 Saving/restoring KB from disk} *)

let save ~file kb =
  let json = to_json kb in
  Json.to_file file json

let restore ~file kb =
  let json = Json.from_file file in
  of_json kb json
