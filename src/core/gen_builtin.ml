(* gen_builtin.ml — generates builtin_gen.ml and builtin_gen.mli
   This file is free software, part of Logtk. See file "license" for more details.
   Build: (rule (targets builtin_gen.ml builtin_gen.mli) (deps gen_builtin.ml)
                (action (run ocaml %{project_root}/src/core/gen_builtin.ml %{targets}))) *)

let out_ml_file = try Sys.argv.(1) with _ -> "builtin_gen.ml"
let out_mli_file = try Sys.argv.(2) with _ -> "builtin_gen.mli"

type fixity =
  | Prefix
  | Infix_binary
  | Infix_nary

type entry = {
  name: string; (* view_t constructor name, e.g. "Not" *)
  hstring: string; (* canonical UTF-8 string, e.g. "¬" *)
  fixity: fixity;
  is_logical: bool;
  is_logical_binop: bool;
  is_arith: bool;
  is_quantifier: bool;
  is_combinator: bool;
  is_flattened: bool;
  tptp: string;
  zf: string;
  code: int;
}

let fixed_builtins : entry list =
  [
    {
      name = "Not";
      hstring = "¬";
      fixity = Prefix;
      is_logical = true;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "~";
      zf = "~";
      code = 2;
    };
    {
      name = "And";
      hstring = "∧";
      fixity = Infix_nary;
      is_logical = true;
      is_logical_binop = true;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = true;
      tptp = "&";
      zf = "&&";
      code = 3;
    };
    {
      name = "Or";
      hstring = "∨";
      fixity = Infix_nary;
      is_logical = true;
      is_logical_binop = true;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = true;
      tptp = "|";
      zf = "||";
      code = 4;
    };
    {
      name = "Imply";
      hstring = "⇒";
      fixity = Infix_binary;
      is_logical = true;
      is_logical_binop = true;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "=>";
      zf = "=>";
      code = 5;
    };
    {
      name = "Equiv";
      hstring = "≡";
      fixity = Infix_binary;
      is_logical = true;
      is_logical_binop = true;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "<=>";
      zf = "<=>";
      code = 6;
    };
    {
      name = "Xor";
      hstring = "<~>";
      fixity = Infix_binary;
      is_logical = true;
      is_logical_binop = true;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "<~>";
      zf = "";
      code = 7;
    };
    {
      name = "Eq";
      hstring = "=";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "=";
      zf = "=";
      code = 8;
    };
    {
      name = "Neq";
      hstring = "≠";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "!=";
      zf = "!=";
      code = 9;
    };
    {
      name = "HasType";
      hstring = ":";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = ":";
      zf = ":";
      code = 10;
    };
    {
      name = "True";
      hstring = "true";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$true";
      zf = "true";
      code = 0;
    };
    {
      name = "False";
      hstring = "false";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$false";
      zf = "false";
      code = 1;
    };
    {
      name = "Arrow";
      hstring = "→";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = ">";
      zf = ">";
      code = 12;
    };
    {
      name = "Wildcard";
      hstring = "_";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$_";
      zf = "_";
      code = 13;
    };
    {
      name = "Multiset";
      hstring = "Ms";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "";
      zf = "";
      code = 14;
    };
    {
      name = "TType";
      hstring = "type";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$tType";
      zf = "type";
      code = 15;
    };
    {
      name = "Prop";
      hstring = "prop";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$o";
      zf = "prop";
      code = 18;
    };
    {
      name = "Term";
      hstring = "ι";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$i";
      zf = "term";
      code = 19;
    };
    {
      name = "ForallConst";
      hstring = "·∀";
      fixity = Prefix;
      is_logical = true;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = true;
      is_combinator = false;
      is_flattened = false;
      tptp = "!!";
      zf = "!!";
      code = 47;
    };
    {
      name = "ExistsConst";
      hstring = "·∃";
      fixity = Prefix;
      is_logical = true;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = true;
      is_combinator = false;
      is_flattened = false;
      tptp = "??";
      zf = "??";
      code = 48;
    };
    {
      name = "ChoiceConst";
      hstring = "·ε";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$$choice";
      zf = "$choice";
      code = 49;
    };
    {
      name = "Grounding";
      hstring = "★";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$$ground";
      zf = "$$grounding";
      code = 50;
    };
    {
      name = "TyInt";
      hstring = "int";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$int";
      zf = "int";
      code = 21;
    };
    {
      name = "TyRat";
      hstring = "rat";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$rat";
      zf = "rat";
      code = 20;
    };
    {
      name = "TyReal";
      hstring = "real";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$real";
      zf = "real";
      code = 70;
    };
    {
      name = "Floor";
      hstring = "floor";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$floor";
      zf = "$floor";
      code = 22;
    };
    {
      name = "Ceiling";
      hstring = "ceiling";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$ceiling";
      zf = "$ceiling";
      code = 23;
    };
    {
      name = "Truncate";
      hstring = "truncate";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$truncate";
      zf = "$truncate";
      code = 24;
    };
    {
      name = "Round";
      hstring = "round";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$round";
      zf = "$round";
      code = 25;
    };
    {
      name = "Prec";
      hstring = "prec";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$prec";
      zf = "$prec";
      code = 26;
    };
    {
      name = "Succ";
      hstring = "succ";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$succ";
      zf = "$succ";
      code = 27;
    };
    {
      name = "Sum";
      hstring = "+";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$sum";
      zf = "+";
      code = 28;
    };
    {
      name = "Difference";
      hstring = "-";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$diff";
      zf = "-";
      code = 29;
    };
    {
      name = "Uminus";
      hstring = "uminus";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$uminus";
      zf = "-";
      code = 30;
    };
    {
      name = "Product";
      hstring = "×";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$product";
      zf = "*";
      code = 31;
    };
    {
      name = "Quotient";
      hstring = "/";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$quotient";
      zf = "$quotient";
      code = 32;
    };
    {
      name = "Quotient_e";
      hstring = "quotient_e";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$quotient_e";
      zf = "/";
      code = 33;
    };
    {
      name = "Quotient_t";
      hstring = "quotient_t";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$quotient_t";
      zf = "$quotient_t";
      code = 34;
    };
    {
      name = "Quotient_f";
      hstring = "quotient_f";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$quotient_f";
      zf = "$quotient_f";
      code = 35;
    };
    {
      name = "Remainder_e";
      hstring = "remainder_e";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$remainder_e";
      zf = "mod";
      code = 36;
    };
    {
      name = "Remainder_t";
      hstring = "remainder_t";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$remainder_t";
      zf = "$remainder_t";
      code = 37;
    };
    {
      name = "Remainder_f";
      hstring = "remainder_f";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$remainder_f";
      zf = "$remainder_f";
      code = 38;
    };
    {
      name = "Is_int";
      hstring = "is_int";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$is_int";
      zf = "$is_int";
      code = 39;
    };
    {
      name = "Is_rat";
      hstring = "is_rat";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$is_rat";
      zf = "$is_rat";
      code = 40;
    };
    {
      name = "To_int";
      hstring = "to_int";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$to_int";
      zf = "$to_int";
      code = 41;
    };
    {
      name = "To_rat";
      hstring = "to_rat";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$to_rat";
      zf = "$to_rat";
      code = 42;
    };
    {
      name = "Less";
      hstring = "<";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$less";
      zf = "<";
      code = 43;
    };
    {
      name = "Lesseq";
      hstring = "≤";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$lesseq";
      zf = "<=";
      code = 44;
    };
    {
      name = "Greater";
      hstring = ">";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$greater";
      zf = ">";
      code = 45;
    };
    {
      name = "Greatereq";
      hstring = "≥";
      fixity = Infix_binary;
      is_logical = false;
      is_logical_binop = false;
      is_arith = true;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$greatereq";
      zf = ">=";
      code = 46;
    };
    {
      name = "Box_opaque";
      hstring = "<box>";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$$box";
      zf = "<box>";
      code = 60;
    };
    {
      name = "BComb";
      hstring = "B";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = true;
      is_flattened = false;
      tptp = "'#B'";
      zf = "B";
      code = 80;
    };
    {
      name = "CComb";
      hstring = "C";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = true;
      is_flattened = false;
      tptp = "'#C'";
      zf = "C";
      code = 81;
    };
    {
      name = "IComb";
      hstring = "I";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = true;
      is_flattened = false;
      tptp = "'#I'";
      zf = "I";
      code = 82;
    };
    {
      name = "KComb";
      hstring = "K";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = true;
      is_flattened = false;
      tptp = "'#K'";
      zf = "K";
      code = 83;
    };
    {
      name = "SComb";
      hstring = "S";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = true;
      is_flattened = false;
      tptp = "'#S'";
      zf = "S";
      code = 84;
    };
    {
      name = "Distinct";
      hstring = "distinct";
      fixity = Prefix;
      is_logical = false;
      is_logical_binop = false;
      is_arith = false;
      is_quantifier = false;
      is_combinator = false;
      is_flattened = false;
      tptp = "$distinct";
      zf = "distinct";
      code = 110;
    };
  ]

let all_names : string list = List.map (fun e -> e.name) fixed_builtins

let entry_of_name : string -> entry =
 fun name -> List.find (fun e -> e.name = name) fixed_builtins

let tptp_to_view : (string * string) list =
  [
    "$true", "True";
    "$false", "False";
    "$_", "Wildcard";
    "$tType", "TType";
    "$i", "Term";
    "$o", "Prop";
    "!!", "ForallConst";
    "??", "ExistsConst";
    "$$choice", "ChoiceConst";
    "$int", "TyInt";
    "$rat", "TyRat";
    "$floor", "Floor";
    "$ceiling", "Ceiling";
    "$truncate", "Truncate";
    "$round", "Round";
    "$prec", "Prec";
    "$succ", "Succ";
    "$sum", "Sum";
    "$difference", "Difference";
    "$uminus", "Uminus";
    "$product", "Product";
    "$quotient", "Quotient";
    "$quotient_e", "Quotient_e";
    "$quotient_t", "Quotient_t";
    "$quotient_f", "Quotient_f";
    "$remainder_e", "Remainder_e";
    "$remainder_t", "Remainder_t";
    "$remainder_f", "Remainder_f";
    "$is_int", "Is_int";
    "$is_rat", "Is_rat";
    "$to_int", "To_int";
    "$to_rat", "To_rat";
    "$less", "Less";
    "$lesseq", "Lesseq";
    "$greater", "Greater";
    "$greatereq", "Greatereq";
    "#B", "BComb";
    "#S", "SComb";
    "#C", "CComb";
    "#K", "KComb";
    "#I", "IComb";
    "$distinct", "Distinct";
    "=", "Eq";
    "!=", "Neq";
    "&", "And";
    "|", "Or";
    "~", "Not";
    "=>", "Imply";
    "<=>", "Equiv";
    "<~>", "Xor";
    ":", "HasType";
    ">", "Arrow";
  ]

(* value constant name → view_t constructor name *)
let const_names : (string * string) list =
  [
    "true_", "True";
    "false_", "False";
    "not_", "Not";
    "and_", "And";
    "or_", "Or";
    "eq", "Eq";
    "neq", "Neq";
    "imply", "Imply";
    "equiv", "Equiv";
    "xor", "Xor";
    "arrow", "Arrow";
    "has_type", "HasType";
    "tType", "TType";
    "prop", "Prop";
    "term", "Term";
    "wildcard", "Wildcard";
    "multiset", "Multiset";
    "ty_int", "TyInt";
    "ty_rat", "TyRat";
    "ty_real", "TyReal";
    "grounding", "Grounding";
    "forallConst", "ForallConst";
    "existsConst", "ExistsConst";
    "choiceConst", "ChoiceConst";
    "floor_", "Floor";
    "ceiling_", "Ceiling";
    "truncate_", "Truncate";
    "round_", "Round";
    "prec_", "Prec";
    "succ_", "Succ";
    "sum_", "Sum";
    "difference_", "Difference";
    "uminus_", "Uminus";
    "product_", "Product";
    "quotient_", "Quotient";
    "quotient_e", "Quotient_e";
    "quotient_t", "Quotient_t";
    "quotient_f", "Quotient_f";
    "remainder_e", "Remainder_e";
    "remainder_t", "Remainder_t";
    "remainder_f", "Remainder_f";
    "is_int_", "Is_int";
    "is_rat_", "Is_rat";
    "to_int_c", "To_int";
    "to_rat_c", "To_rat";
    "less_", "Less";
    "lesseq_", "Lesseq";
    "greater_", "Greater";
    "greatereq_", "Greatereq";
    "box_opaque", "Box_opaque";
    "bComb", "BComb";
    "cComb", "CComb";
    "iComb", "IComb";
    "kComb", "KComb";
    "sComb", "SComb";
    "distinct", "Distinct";
  ]

let () =
  let out_mli = open_out out_mli_file in
  let out_ml = open_out out_ml_file in
  let mli fmt = Printf.fprintf out_mli fmt in
  let ml fmt = Printf.fprintf out_ml fmt in

  mli "(* Auto-generated by gen_builtin.ml -- do not edit directly *)\n\n";
  ml "(* Auto-generated by gen_builtin.ml -- do not edit directly *)\n\n";

  (* view_t *)
  mli "type view_t =\n";
  ml "type view_t =\n";
  List.iter
    (fun n ->
      mli "  | %s\n" n;
      ml "  | %s\n" n)
    all_names;
  mli "\n";
  ml "\n";

  (* fixity *)
  mli "type fixity = Infix_binary | Infix_nary | Prefix\n";
  ml "type fixity = Infix_binary | Infix_nary | Prefix\n\n";

  (* Constants *)
  mli "(* Fixed builtin value constants *)\n";
  List.iter
    (fun (vname, cname) ->
      mli "val %s : Hstring.t\n" vname;
      ml "let %s = Hstring.make %S\n" vname (entry_of_name cname).hstring)
    const_names;
  mli "\n";
  ml "\n";

  (* Arith *)
  let is_arith_name n = (entry_of_name n).is_arith in
  mli "module Arith : sig\n";
  List.iter
    (fun (vname, cname) ->
      if is_arith_name cname then mli "  val %s : Hstring.t\n" vname)
    const_names;
  mli "end\n\n";
  ml "module Arith = struct\n";
  List.iter
    (fun (vname, cname) ->
      if is_arith_name cname then ml "  let %s = %s\n" vname vname)
    const_names;
  ml "end\n\n";

  (* view / make_view / view_order *)
  mli "val view : Hstring.t -> view_t option\n";
  mli "val make_view : view_t -> Hstring.t\n";
  mli "val view_order : view_t -> int\n\n";
  ml "let view_tbl : (Hstring.t, view_t) Hashtbl.t = Hashtbl.create 73\n";
  List.iter
    (fun e ->
      ml "let () = Hashtbl.add view_tbl (Hstring.make %S) %s\n" e.hstring e.name)
    fixed_builtins;
  ml
    "\n\
     let view (h : Hstring.t) : view_t option =\n\
    \  try Some (Hashtbl.find view_tbl h)\n\
    \  with Not_found -> None\n\n";
  ml "let make_view_tbl : (view_t, Hstring.t) Hashtbl.t = Hashtbl.create 73\n";
  List.iter
    (fun e ->
      ml "let () = Hashtbl.add make_view_tbl %s (Hstring.make %S)\n" e.name
        e.hstring)
    fixed_builtins;
  ml
    "\n\
     let make_view (v : view_t) : Hstring.t =\n\
    \  Hashtbl.find make_view_tbl v\n\n";
  ml "let view_order (v : view_t) : int =\n  match v with\n";
  List.iter (fun e -> ml "  | %s -> %d\n" e.name e.code) fixed_builtins;
  ml "\n";

  (* Classification predicates *)
  let classify field fname =
    mli "val %s : Hstring.t -> bool\n" fname;
    ml "let %s (h : Hstring.t) : bool =\n  match view h with\n" fname;
    List.iter
      (fun e ->
        let v =
          match field with
          | "is_logical" -> e.is_logical
          | "is_logical_binop" -> e.is_logical_binop
          | "is_flattened" -> e.is_flattened
          | "is_quantifier" -> e.is_quantifier
          | "is_combinator" -> e.is_combinator
          | "is_arith" -> e.is_arith
          | _ -> false
        in
        if v then ml "  | Some %s -> true\n" e.name)
      fixed_builtins;
    ml "  | _ -> false\n\n"
  in
  classify "is_logical" "is_logical_op";
  classify "is_logical_binop" "is_logical_binop";
  classify "is_flattened" "is_flattened_logical";
  classify "is_quantifier" "is_quantifier";
  classify "is_combinator" "is_combinator";
  classify "is_arith" "is_arith";

  (* fixity *)
  mli "val fixity : Hstring.t -> fixity\n";
  mli "val is_prefix : Hstring.t -> bool\n";
  mli "val is_infix : Hstring.t -> bool\n\n";
  ml "let fixity (h : Hstring.t) : fixity =\n  match view h with\n";
  List.iter
    (fun e ->
      let fx =
        match e.fixity with
        | Prefix -> "Prefix"
        | Infix_binary -> "Infix_binary"
        | Infix_nary -> "Infix_nary"
      in
      ml "  | Some %s -> %s\n" e.name fx)
    fixed_builtins;
  ml "  | None -> Prefix\n\n";
  ml "let is_prefix (h : Hstring.t) : bool = fixity h = Prefix\n";
  ml "let is_infix (h : Hstring.t) : bool = fixity h <> Prefix\n\n";

  (* is_not_numeric *)
  mli "val is_not_numeric : Hstring.t -> bool\n\n";
  ml "let is_not_numeric (h : Hstring.t) : bool =\n  match view h with\n";
  List.iter (fun e -> ml "  | Some %s -> true\n" e.name) fixed_builtins;
  ml "  | None -> false\n\n";

  (* to_string, pp *)
  mli "val to_string : Hstring.t -> string\n";
  mli "val pp : Hstring.t CCFormat.printer\n\n";
  ml "let to_string (h : Hstring.t) : string =\n  match view h with\n";
  List.iter (fun e -> ml "  | Some %s -> %S\n" e.name e.hstring) fixed_builtins;
  ml "  | None -> Hstring.to_string h\n\n";
  ml "let pp out h = Format.pp_print_string out (to_string h)\n\n";

  (* to_int_ *)
  mli "val to_int_ : Hstring.t -> int\n\n";
  ml "let to_int_ (h : Hstring.t) : int =\n  match view h with\n";
  List.iter (fun e -> ml "  | Some %s -> %d\n" e.name e.code) fixed_builtins;
  ml "  | None -> -1\n\n";

  (* TPTP *)
  mli
    "module TPTP : sig\n\
    \  val to_string : Hstring.t -> string\n\
    \  val pp : Hstring.t CCFormat.printer\n";
  mli "  val of_string : string -> (view_t, string) result\n";
  mli
    "  val fixity : view_t -> fixity\n\
    \  val is_prefix : view_t -> bool\n\
    \  val is_infix : view_t -> bool\n";
  mli
    "  val connectives : Hstring.Set.t\n\
    \  val is_connective : Hstring.t -> bool\n\
     end\n\n";

  ml
    "module TPTP = struct\n\
    \  let to_string (h : Hstring.t) : string =\n\
    \    match view h with\n";
  List.iter (fun e -> ml "    | Some %s -> %S\n" e.name e.tptp) fixed_builtins;
  ml "    | None -> Hstring.to_string h\n\n";
  ml "  let pp out h = Format.pp_print_string out (to_string h)\n\n";
  ml "  let of_string s : (view_t, string) result =\n    match s with\n";
  List.iter (fun (s, n) -> ml "    | %S -> Ok %s\n" s n) tptp_to_view;
  ml "    | _ -> Error (Printf.sprintf \"not a TPTP builtin: %%s\" s)\n\n";
  ml "  let fixity (v : view_t) : fixity =\n    match v with\n";
  List.iter
    (fun e ->
      let fx =
        match e.fixity with
        | Prefix -> "Prefix"
        | Infix_binary -> "Infix_binary"
        | Infix_nary -> "Infix_nary"
      in
      ml "    | %s -> %s\n" e.name fx)
    fixed_builtins;
  ml
    "\n\
    \  let is_prefix v = fixity v = Prefix\n\
    \  let is_infix v = fixity v <> Prefix\n\n";
  ml
    "  let connectives : Hstring.Set.t =\n\
    \    Hstring.Set.of_iter (Iter.of_list [and_; or_; equiv; imply])\n\n";
  ml
    "  let is_connective (h : Hstring.t) : bool =\n\
    \    match view h with Some (And | Or | Equiv | Imply) -> true | _ -> false\n\
     end\n\n";

  (* ZF *)
  mli
    "module ZF : sig\n\
    \  val to_string : Hstring.t -> string\n\
    \  val pp : Hstring.t CCFormat.printer\n\
     end\n\n";
  ml
    "module ZF = struct\n\
    \  let to_string (h : Hstring.t) : string =\n\
    \    match view h with\n";
  List.iter (fun e -> ml "    | Some %s -> %S\n" e.name e.zf) fixed_builtins;
  ml "    | None -> Hstring.to_string h\n\n";
  ml "  let pp out h = Format.pp_print_string out (to_string h)\nend\n\n";

  close_out out_mli;
  close_out out_ml
