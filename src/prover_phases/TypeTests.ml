(* A dynamic type test is a predicate 'a->bool. They work by traversing the term structure. Since types are erased, same data can have multiple types. But if a type test T on data X is succesful, then at least X can be (magic) cast to T safely. Safety means no segmentation fault but risks custom invariants. The below tests are accure in this sense, or preserve accuracy, except the unsafe tests integer/Z.t, rational/Q.t and lazy_or. There's no test combinators for polymorphic variants and exceptions—they can be accurately tested by pattern matching.
 Currently this file contains all definitions of type tests and their associations to string printers. It is hence easy to disable this debugging by never registering the corresponding extension. Another design would be to scatter the type tests next to the corresponding real type definitions. That'd reduce the risk of forgetting to update a test along side its corresponding type. *)

open Logtk
open Libzipperposition
open List
open Obj (* OCaml's reflection module—powers all of this *)
open Util
open Util.UntypedPrint
open CCFun

(* Self-debugging
Note: to use debug_has_type you must prevent cyclic dependency between this file and the file you debug. Temporarily moving registration of printers from this file to a new file offers a possible solution. *)

let debug_typing_fail = ref `Off
(* The root type test combinators are annotated by this to record their last failure. *)
let debug_root x b = Printexc.(if not b then match !debug_typing_fail with 
  |`At(_,tr0)->
      let tr1 = get_callstack 9999 in
      (* Do not replace a stacktrace by its subtrace, so compare slot arrays. *)
      let sl0, sl1 = CCPair.map_same (CCArray.rev % CCOpt.get_exn % backtrace_slots) (tr0, tr1) in
      if sl1 = Array.(sub sl0 0 (min (length sl0) (length sl1))) then
        debug_typing_fail := `At((magic x : any), tr1)
  |`Off -> ()
  |`On -> debug_typing_fail := `At(magic x, get_callstack 9999)
); b

(* When you see garbage output from str, ~< or |< instead of registered pretty printer's output, then the corresponding type test has an error. Use this function to narrow which subexpression is causing the problem. So replace, say ~<x, by debug_has_type t x where t is the type test that should pass for x. *)
let debug_has_type t x =
  debug_typing_fail := `On;
  let x_is_t = t x in
  let recorded = !debug_typing_fail in
  debug_typing_fail := `Off;
  if x_is_t then (print_endline("Valid typing in " ^ caller_file_line 1); x) else(
    print_endline("Invalid typing for value:\n" ^ str x ^ (match recorded with
      | `At(sub_x, trace) ->
        "\n\nTyping the following subexpression failed:\n" ^ str sub_x ^
        "\n\nStack trace of the last corresponding test:\n" ^ Printexc.raw_backtrace_to_string trace
      | _(*`On *) ->
        "\n\nThe failing subtest is a custom one such as pattern match. You must locate it by other means, sorry."
      )^"\n\nPress enter to continue the run . . .");
    let _=read_line() in x)


(* Helpers *)

let fields x = init (size(repr x)) (magic(field(repr x)))

let test (c:int) f x = let x = magic x in debug_root x (
  if is_int x then 0<=x & x<c else
  let t = tag x in f(t, if t=out_of_heap_tag or t=unaligned_tag then [] else fields x))

let test_tag tag f = test 0 (fun(tag',data) -> tag=tag' & f data)


(* Type tests for builtin types and type constructs *)

let any _ = true
let int x = debug_root x (is_int(repr x))

(* Test an algebraic data type: c is the number of constant constructors, and ttt is a list of lists of type tests for the parameters of other constructors, all in order of appearence. *)
let union c ttt = test c (fun(tag,data) -> tag < length ttt & for_all_2 id (nth ttt tag) data)
let enum c = union c []

let rec list t x = union 1 [[t; list t]] x
(* Warning: opt(tuple[...]) doesn't substitute union 1 [[...]] despite OCaml's syntax. *)
let opt t = union 1 [[t]]

let bool x = enum 2 x
(* Test flat tuples and records: tt is a list of type tests for the components, in order of appearence, and ~st is an optional extra condition for inter-field invariants after tt was succesful. *)
let tuple tt ?(st=fun _->true) x = test_tag 0 (for_all_2 id tt) x
  & debug_root x (st(magic x))
let decimal x = test_tag double_tag any x
let array t = test_tag (if t==decimal then double_array_tag else 0) (for_all t)
let string x = test_tag string_tag any x
let func x = test_tag closure_tag any x
let custom x = test_tag custom_tag any x or test_tag abstract_tag any x (* e.g. int32, big Z.t *)
(* Use lazy_force unless there's values that cannot be eagerly computed or you never need the lazy value. The lazy_or does not check the type of an unevaluated expression.
Reference: https://stackoverflow.com/questions/56746374/how-does-ocaml-represent-lazy-values-at-runtime *)
let lazy_aid f t x = test_tag lazy_tag f x or test_tag forward_tag (t % hd) x or t x
let lazy_or t = lazy_aid any t
let lazy_force t x = lazy_aid (fun _-> t(Lazy.force(magic x))) t x
(* First field of an object would be “class” and second an “id”. E.g. modules might be objects in addition to being at least tuples and closures.
Open questions: Is class always string? Is id always int? Is there ever more fields? Which OCaml concepts translate to objects? *)
let object0 x = test_tag object_tag (for_all_2 id [string;int]) x
let exception' x = object0 x or test_tag 0 (function y::_-> object0 y |_->false) x

(* Common non-primitive types *)

(* Test for the default Map of OCaml which as a type is also the same as CCMap. *)
let rec ccmap key t x = union 1 [[ccmap key t; key; t; ccmap key t; int]] x
(* Test for the default Set of OCaml which as a type is also the same as CCSet. *)
let rec ccset t x = union 1 [[ccset t; t; ccset t; int]] x
let ccbv x = tuple[array int; int] x
let ccvector t = tuple[int; array t] ~st:(fun(s,a)-> size a >= s & s >= 0)
let rec bucketlist k v x = union 1 [[k; v; bucketlist k v]] x
let hashtbl k v = tuple[int; array(bucketlist k v); int; int] ~st:(fun(s,a,_,_) -> size a >= s/2 & s >= 0)

(* arbitrary precision types—unsafe! *)
let integer x = int x or custom x
let rational x = tuple[integer; integer] x

(* Types specific to Zipperposition *)

let builtin x = union 57 [[integer]; [rational]; [int]] x
let an_id x = tuple[int; string; list exception'] x
let hvar t = tuple[int;t]
let rec term x = tuple[view; opt term; int; exception'; custom; lazy_force int] x
and view x = union 0 [
  [tvar];
  [int];
  [enum 4; term; term];
  [an_id];
  [term; list term];
  [builtin; list term]] x
and tvar x = hvar term x

let num_class t = tuple[
  term;
  func;func;func;func;func;func;
  t;t;
  func;func;func;func;func;func;func;func;func]
let monome t = tuple[num_class t; t; list(tuple[t;term])]
let int_literal x = union 0 [
  [enum 4; monome integer; monome integer];
  [tuple[integer; int; monome integer; bool]]] x
let rat_literal x = tuple[bool; monome rational; monome rational] x
let literal x = union 2 [[term; term; bool]; [int_literal]; [rat_literal]] x
let literals x = array literal x
let sliteral t x = union 2 [[t;bool]; [t;t]; [t;t]] x

let theory_tag x = union 11 [[an_id]] x
let parse_location x = tuple[string; int; int; int; int] x
let from_file x = tuple[string; opt string; opt parse_location] x
let rec attrs x = list(union 0 [[string; attrs]; [string]; [attrs]]) x
let source x = tuple[int; union 0 [[from_file; attrs]; [attrs]]] x
let kind x = union 1 [
  [source; enum 5];
  [string; list theory_tag];
  [string; list theory_tag];
  [string];
  [an_id; source];
  [an_id]] x

let scoped t = tuple[t;int]
let subst x = ccmap (scoped tvar) (scoped term) x
let renaming x = union 1 [[ccmap (scoped tvar) tvar; int]] x
let subst_projection x = tuple[int; subst; renaming] x
let inf_result x = tuple[tuple[
  int; func;func;func; union 2 []; func;func;func;func; opt func; func
]; exception'] x

let rec proof_step x = tuple[int; kind; opt int; int; list parent; attrs] x
and parent x = union 0 [[proof]; [proof; subst_projection]] x
and proof x = tuple[proof_step; inf_result] x

let rec constructor x = tuple[an_id; term; list(tuple[term; projector])] x
and projector x = tuple[an_id; term; int; lazy_force constructor] x
let ind_type x = tuple[an_id; list tvar; term; list constructor; lazy_force bool; proof] x
let ind_cst x = tuple[an_id; list term; term; ind_type; bool; int] x
let inductive_case x = tuple[ind_cst; term; (function`Base|`Rec->true|_->false); list ind_cst; list(tuple[an_id;term])] x

let rec position x = union 1 [[position]; [position]; [position]; [position]; [int;position]; [position]] x
let cut_form x = tuple[ccset tvar; list literals] x
let payload x = union 1 [[literals]; [cut_form]; [list inductive_case]] x
let trail x = ccset(tuple[int; payload; any(*TODO What should this be? See bool_lit.ml line 20*)]) x
let sclause x = tuple[int; literals; trail; int] x
let clause x = tuple[
  sclause;
  int; 
  lazy_force ccbv;
  lazy_force(list(tuple[term; position]));
  lazy_force(list int);
  proof_step;
  opt ccbv;
  opt(ccset(tuple[term; position]))] x

let szs_status x = union 3 [[proof]; [string]] x
let or_error t = union 0 [[t]; [string]]
let exn_pair x = tuple[tuple[object0;int]; exception'] x
let flex_state x = ccmap int exn_pair x
let run's_result x = or_error(tuple[flex_state; szs_status]) x

(* let powers x = array int x
let shifts x = union 1 [
  [powers; term];
  [powers; term; powers; term];
  [int; int; powers; term]] x
let monomial x = tuple[integer; powers; shifts] x
let polynomial x = list monomial x *)

let rec polynomial x = list monomial x
and monomial x = list indeterminate x
and indeterminate x = union 0 [
  [integer];
  [op_atom];
  [int];
  [int];
  [int];
  [monomial];
  [list(tuple[int;polynomial])]] x
and op_atom x = union 1 [[int]; [term]] x


(* Define and register string converters *)

let indent = ref "\n"
let clever_view sep view l = String.(
  indent := !indent^" ";
  let l = List.map view l in
  let s = concat (sep^" ") l in
  (* If s is long and not about all entries of l are short, then put each on its own line. *)
  let s' = if length s < 100 or List.length(filter ((<)9 % length) l) < 3
    then s else concat (sep ^ !indent) l in
  indent := sub !indent 0 (length !indent - 1);
  s')
(* optional helper to print integers, symbolic numbers, ring elements etc. differently *)
let serif, double, sans, bold, thin = 94,104,114,124,134
let digits_in style = String.(to_seq %> List.of_seq %> concat_view "" (function
  | '0'..'9' as d -> "\xF0\x9D\x9F" ^ make 1 Char.(chr(style + code d))
  | c -> make 1 c))

let extension = {Extensions.default with
  name = "debug type tests";
  env_actions = [fun env -> let module Env = (val env) in 

add_pp clause (CCFormat.to_string Env.C.pp_tstp);
]};;

(* Do overly general assignments first so they end up to the bottom of the printer stack. Above env_actions run of course after this file. *)

add_pp any (fun x -> (match tag x with
  | t when t=final_tag -> "final" 
  | t when t=out_of_heap_tag -> "code"
  | t when t=unaligned_tag -> "unaligned" (* e.g. 1ˢᵗ closure field unless its the above *)
  | _ -> "tag="^str(tag x)
)^":size="^str(size x));

add_pp (test 0 (fun(tag,_) -> tag < no_scan_tag)) (fun x -> 
  superscript(str(size x)) (* Prepend length of data tuple for easier exploration. *)
  ^ (match tag x with 0-> "" | t-> "tag" ^ str t) (* Omit multipurpose default tag 0. *)
  ^ "("^ clever_view "," str (fields x) ^")");

add_pp (ccvector any) (CCVector.to_string ~start:"ᵛᵉᶜ⟨" ~stop:"⟩" str);

add_pp (list any) (fun l -> "["^ clever_view ";" str l ^"]");

add_pp (ccset any) (
  let rec to_list s = if s == repr 0 then [] else to_list(field s 0) @ field s 1 :: to_list(field s 2) in
  fun s -> "{"^ clever_view "," str (to_list s) ^"}");

add_pp (hashtbl any any) Hashtbl.(fun t -> "{"^if length t = 0 then "↦̸ }" else
  clever_view ";" (fun(k,v)-> str k ^"↦ "^ str v) (List.of_seq(to_seq t)) ^"}");

(* already evaluated lazy values *)
add_pp (test_tag forward_tag any) (str % Lazy.force);

add_pp (lazy_or(fun _->false)) (fun _->"lazy");

(* The infix_tag is for mutually recursive functions acording to a comment on https://github.com/ocaml/ocaml/issues/7810 *)
add_pp (test 0 (fun(tag,_) -> tag=closure_tag or tag=infix_tag)) (fun f -> 
  match fields f with
  (* First field is code pointer and subsequent are captured parameters and auxiliarities. The separated case of int+pointer often occurs as the last field, and is made less noisy. *)
  | [_;n;f] when int n & tag f = out_of_heap_tag -> "function" ^ str n
  | _::p::pp -> "closure("^ clever_view "," str (p::pp) ^")"
  | _ -> "function");

(* Do specific non-parametric assignments *)

add_pp run's_result (function
| CCResult.Error info -> "🚫 "^info
| CCResult.Ok(state, res) ->
  Saturate.(match res with
    | Unsat proof -> "Proof:\n"^ str proof
    | Sat -> "Satisfiable"
    | Unknown -> "Unknown"
    | Error info -> "OK but "^info
    | Timeout -> "Timeout"
  )^" – with flex_state {"^ (* Don't know if useful but print flex_state anyway. *)
    CCHet.(Map.to_list state |> clever_view ";" (fun(Pair(_,e)) -> str e)) ^"}");

add_pp exception' Printexc.(fun e ->
  exn_slot_name e ^"#"^ str(exn_slot_id e) ^ if tuple[any;any] e then
    "("^ clever_view "," str (tl(fields e)) ^")"
  else "");

add_pp decimal string_of_float;
(* Quote number and invisible strings. *)
add_pp string (fun s -> if None != int_of_string_opt(String.trim s ^ "0") then "“"^s^"”" else s);
(* Do not print list [term] as SLiteral ¬term. *)
add_pp (fun x -> sliteral term x & not(list any x)) ((^)"ᔆᴸⁱᵗ" % SLiteral.to_string Term.pp);
add_pp tvar HVar.to_string;
add_pp subst Subst.to_string;
add_pp term Term.to_string;
add_pp literal Literal.to_string;
add_pp proof (CCFormat.to_string Proof.S.pp_normal);

add_pp monomial (digits_in serif % RecurrencePolynomial.mono_to_string);
add_pp polynomial (digits_in serif % RecurrencePolynomial.poly_to_string);
(* open Summation_equality.RecurrencePolynomial;;
add_pp monomial mono_to_string;
add_pp polynomial poly_to_string; *)