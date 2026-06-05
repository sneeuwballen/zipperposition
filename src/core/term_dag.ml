module E = Zipperposition_mdag.Encode
module D = Zipperposition_mdag.Decode

(* Encoding format — each node stores its type annotation as the last value:
   - "tm.var"      int ref(ty)            — HVar.id, type term
   - "tm.db"       int ref(ty)            — db index, type term
   - "tm.lam"      ref(varty) ref(body) ref(ty)
   - "tm.forall"   ref(varty) ref(body) ref(ty)
   - "tm.forallty" ref(varty) ref(body) ref(ty)
   - "tm.exists"   ref(varty) ref(body) ref(ty)
   - "tm.const"    string ref(ty)         — name, type term
   - "tm.app"      ref(f) ref(arg)* ref(ty)
   - "tm.builtin"  string (string|null) ref(arg)* ref(ty)

   NoType is encoded as null (which decodes to InnerTerm.tType as a sentinel). *)

type t = {
  enc: E.t;
  term_cache: E.offset InnerTerm.WeakTbl.t;
}

let create enc = { enc; term_cache = InnerTerm.WeakTbl.create 64 }

let builtin_to_strings (b : Builtin.t) : string * string option =
  match b with
  | Builtin.Int n -> "Int", Some (Z.to_string n)
  | Builtin.Rat q -> "Rat", Some (Q.to_string q)
  | Builtin.Real s -> "Real", Some s
  | Builtin.Pseudo_de_bruijn i -> "Pseudo_de_bruijn", Some (string_of_int i)
  | Builtin.Not -> "Not", None
  | Builtin.And -> "And", None
  | Builtin.Or -> "Or", None
  | Builtin.Imply -> "Imply", None
  | Builtin.Equiv -> "Equiv", None
  | Builtin.Xor -> "Xor", None
  | Builtin.Eq -> "Eq", None
  | Builtin.Neq -> "Neq", None
  | Builtin.HasType -> "HasType", None
  | Builtin.True -> "True", None
  | Builtin.False -> "False", None
  | Builtin.Arrow -> "Arrow", None
  | Builtin.Wildcard -> "Wildcard", None
  | Builtin.Multiset -> "Multiset", None
  | Builtin.TType -> "TType", None
  | Builtin.Prop -> "Prop", None
  | Builtin.Term -> "Term", None
  | Builtin.ForallConst -> "ForallConst", None
  | Builtin.ExistsConst -> "ExistsConst", None
  | Builtin.ChoiceConst -> "ChoiceConst", None
  | Builtin.Grounding -> "Grounding", None
  | Builtin.TyInt -> "TyInt", None
  | Builtin.TyRat -> "TyRat", None
  | Builtin.TyReal -> "TyReal", None
  | Builtin.Floor -> "Floor", None
  | Builtin.Ceiling -> "Ceiling", None
  | Builtin.Truncate -> "Truncate", None
  | Builtin.Round -> "Round", None
  | Builtin.Prec -> "Prec", None
  | Builtin.Succ -> "Succ", None
  | Builtin.Sum -> "Sum", None
  | Builtin.Difference -> "Difference", None
  | Builtin.Uminus -> "Uminus", None
  | Builtin.Product -> "Product", None
  | Builtin.Quotient -> "Quotient", None
  | Builtin.Quotient_e -> "Quotient_e", None
  | Builtin.Quotient_t -> "Quotient_t", None
  | Builtin.Quotient_f -> "Quotient_f", None
  | Builtin.Remainder_e -> "Remainder_e", None
  | Builtin.Remainder_t -> "Remainder_t", None
  | Builtin.Remainder_f -> "Remainder_f", None
  | Builtin.Is_int -> "Is_int", None
  | Builtin.Is_rat -> "Is_rat", None
  | Builtin.To_int -> "To_int", None
  | Builtin.To_rat -> "To_rat", None
  | Builtin.Less -> "Less", None
  | Builtin.Lesseq -> "Lesseq", None
  | Builtin.Greater -> "Greater", None
  | Builtin.Greatereq -> "Greatereq", None
  | Builtin.Box_opaque -> "Box_opaque", None
  | Builtin.BComb -> "BComb", None
  | Builtin.CComb -> "CComb", None
  | Builtin.IComb -> "IComb", None
  | Builtin.KComb -> "KComb", None
  | Builtin.SComb -> "SComb", None
  | Builtin.Distinct -> "Distinct", None

let strings_to_builtin (name : string) (payload : string option) : Builtin.t =
  match name, payload with
  | "Int", Some s -> Builtin.Int (Z.of_string s)
  | "Rat", Some s -> Builtin.Rat (Q.of_string s)
  | "Real", Some s -> Builtin.Real s
  | "Pseudo_de_bruijn", Some s -> Builtin.Pseudo_de_bruijn (int_of_string s)
  | "Not", None -> Builtin.Not
  | "And", None -> Builtin.And
  | "Or", None -> Builtin.Or
  | "Imply", None -> Builtin.Imply
  | "Equiv", None -> Builtin.Equiv
  | "Xor", None -> Builtin.Xor
  | "Eq", None -> Builtin.Eq
  | "Neq", None -> Builtin.Neq
  | "HasType", None -> Builtin.HasType
  | "True", None -> Builtin.True
  | "False", None -> Builtin.False
  | "Arrow", None -> Builtin.Arrow
  | "Wildcard", None -> Builtin.Wildcard
  | "Multiset", None -> Builtin.Multiset
  | "TType", None -> Builtin.TType
  | "Prop", None -> Builtin.Prop
  | "Term", None -> Builtin.Term
  | "ForallConst", None -> Builtin.ForallConst
  | "ExistsConst", None -> Builtin.ExistsConst
  | "ChoiceConst", None -> Builtin.ChoiceConst
  | "Grounding", None -> Builtin.Grounding
  | "TyInt", None -> Builtin.TyInt
  | "TyRat", None -> Builtin.TyRat
  | "TyReal", None -> Builtin.TyReal
  | "Floor", None -> Builtin.Floor
  | "Ceiling", None -> Builtin.Ceiling
  | "Truncate", None -> Builtin.Truncate
  | "Round", None -> Builtin.Round
  | "Prec", None -> Builtin.Prec
  | "Succ", None -> Builtin.Succ
  | "Sum", None -> Builtin.Sum
  | "Difference", None -> Builtin.Difference
  | "Uminus", None -> Builtin.Uminus
  | "Product", None -> Builtin.Product
  | "Quotient", None -> Builtin.Quotient
  | "Quotient_e", None -> Builtin.Quotient_e
  | "Quotient_t", None -> Builtin.Quotient_t
  | "Quotient_f", None -> Builtin.Quotient_f
  | "Remainder_e", None -> Builtin.Remainder_e
  | "Remainder_t", None -> Builtin.Remainder_t
  | "Remainder_f", None -> Builtin.Remainder_f
  | "Is_int", None -> Builtin.Is_int
  | "Is_rat", None -> Builtin.Is_rat
  | "To_int", None -> Builtin.To_int
  | "To_rat", None -> Builtin.To_rat
  | "Less", None -> Builtin.Less
  | "Lesseq", None -> Builtin.Lesseq
  | "Greater", None -> Builtin.Greater
  | "Greatereq", None -> Builtin.Greatereq
  | "Box_opaque", None -> Builtin.Box_opaque
  | "BComb", None -> Builtin.BComb
  | "CComb", None -> Builtin.CComb
  | "IComb", None -> Builtin.IComb
  | "KComb", None -> Builtin.KComb
  | "SComb", None -> Builtin.SComb
  | "Distinct", None -> Builtin.Distinct
  | _ ->
    failwith (Printf.sprintf "Term_dag.strings_to_builtin: unknown %S" name)

let rec encode_type_offset (self : t) (ty : InnerTerm.type_result) :
    E.offset option =
  match ty with
  | NoType -> None
  | HasType t -> Some (encode_term self t)

and encode_term (self : t) (term : InnerTerm.t) : E.offset =
  match InnerTerm.WeakTbl.find self.term_cache term with
  | offset -> offset
  | exception Not_found ->
    let offset =
      match InnerTerm.view term with
      | Var hv ->
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc "tm.var" (fun nd ->
            E.int nd (HVar.id hv);
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
      | DB n ->
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc "tm.db" (fun nd ->
            E.int nd n;
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
      | Bind (binder, varty, body) ->
        let cmd =
          match binder with
          | Binder.Lambda -> "tm.lam"
          | Binder.Forall -> "tm.forall"
          | Binder.ForallTy -> "tm.forallty"
          | Binder.Exists -> "tm.exists"
        in
        let varty_off = encode_term self varty in
        let body_off = encode_term self body in
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc cmd (fun nd ->
            E.ref nd varty_off;
            E.ref nd body_off;
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
      | Const id ->
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc "tm.const" (fun nd ->
            E.string nd (Hstring.to_string id);
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
      | App (f, args) ->
        let f_off = encode_term self f in
        let arg_offs = List.map (encode_term self) args in
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc "tm.app" (fun nd ->
            E.ref nd f_off;
            List.iter (E.ref nd) arg_offs;
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
      | AppBuiltin (b, args) ->
        let name, payload = builtin_to_strings b in
        let arg_offs = List.map (encode_term self) args in
        let ty_off = encode_type_offset self (InnerTerm.ty term) in
        E.write_node self.enc "tm.builtin" (fun nd ->
            E.string nd name;
            (match payload with
            | None -> E.null nd
            | Some p -> E.string nd p);
            List.iter (E.ref nd) arg_offs;
            match ty_off with
            | None -> E.null nd
            | Some off -> E.ref nd off)
    in
    InnerTerm.WeakTbl.replace self.term_cache term offset;
    offset

let encoder_flush (self : t) = E.flush self.enc

(* -- Decoding -- *)

type decode_state = {
  dec: D.t;
  mutable terms: InnerTerm.t option array;
}

let rec decode_term_at (st : decode_state) (off : int) : InnerTerm.t =
  match st.terms.(off) with
  | Some t -> t
  | None ->
    let t = D.read_node st.dec off (fun nd cmd -> decode_term_node st nd cmd) in
    st.terms.(off) <- Some t;
    t

and decode_type_from_value (st : decode_state) (v : D.value) :
    InnerTerm.type_result =
  match v with
  | D.Null -> NoType
  | D.Ref off -> HasType (decode_term_at st off)
  | _ -> failwith "Term_dag.decode: expected null or ref for type annotation"

and decode_term_from_value (st : decode_state) (v : D.value) : InnerTerm.t =
  match v with
  | D.Ref off -> decode_term_at st off
  | D.Null -> InnerTerm.tType
  | _ -> failwith "Term_dag.decode: expected ref for term"

and read_remaining_values (nd : D.node_decoder) : D.value list =
  let rec go acc =
    match D.read nd with
    | D.Stop -> List.rev acc
    | v -> go (v :: acc)
  in
  go []

and decode_term_node (st : decode_state) (nd : D.node_decoder) (cmd : string) :
    InnerTerm.t =
  match cmd with
  | "tm.var" ->
    let vals = read_remaining_values nd in
    let id =
      match vals with
      | D.Int64 n :: _ -> Int64.to_int n
      | _ -> failwith "Term_dag.decode: tm.var expected int as first arg"
    in
    let ty =
      match List.rev vals with
      | ty_v :: _ -> decode_type_from_value st ty_v
      | [] -> NoType
    in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> failwith "Term_dag.decode: tm.var requires a type"
    in
    let hv : InnerTerm.t HVar.t = HVar.make_unsafe ~ty:ty_term id in
    InnerTerm.var hv
  | "tm.db" ->
    let vals = read_remaining_values nd in
    let n =
      match vals with
      | D.Int64 n :: _ -> Int64.to_int n
      | _ -> failwith "Term_dag.decode: tm.db expected int as first arg"
    in
    let ty =
      match List.rev vals with
      | ty_v :: _ -> decode_type_from_value st ty_v
      | [] -> NoType
    in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> failwith "Term_dag.decode: tm.db requires a type"
    in
    InnerTerm.bvar ~ty:ty_term n
  | ("tm.lam" | "tm.forall" | "tm.forallty" | "tm.exists") as binder_cmd ->
    let vals = read_remaining_values nd in
    let binder =
      match binder_cmd with
      | "tm.lam" -> Binder.Lambda
      | "tm.forall" -> Binder.Forall
      | "tm.forallty" -> Binder.ForallTy
      | "tm.exists" -> Binder.Exists
      | _ -> assert false
    in
    let ty_v, varty_v, body_v =
      match List.rev vals with
      | ty_v :: body_v :: varty_v :: _ -> ty_v, varty_v, body_v
      | _ -> failwith "Term_dag.decode: bind expected 3 values"
    in
    let varty = decode_term_from_value st varty_v in
    let body = decode_term_from_value st body_v in
    let ty = decode_type_from_value st ty_v in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> failwith "Term_dag.decode: bind requires a type"
    in
    InnerTerm.bind ~ty:ty_term ~varty binder body
  | "tm.const" ->
    let vals = read_remaining_values nd in
    let name =
      match vals with
      | D.String s :: _ -> s
      | _ -> failwith "Term_dag.decode: tm.const expected string"
    in
    let ty =
      match List.rev vals with
      | ty_v :: _ -> decode_type_from_value st ty_v
      | [] -> NoType
    in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> failwith "Term_dag.decode: tm.const requires a type"
    in
    let id = Hstring.make name in
    InnerTerm.const ~ty:ty_term id
  | "tm.app" ->
    let vals = read_remaining_values nd in
    let ty_v, term_vals =
      match List.rev vals with
      | ty_v :: rest -> ty_v, List.rev rest
      | [] -> failwith "Term_dag.decode: tm.app expected at least one value"
    in
    let f =
      match term_vals with
      | f_v :: _ -> decode_term_from_value st f_v
      | [] -> failwith "Term_dag.decode: tm.app expected function"
    in
    let args = List.tl term_vals |> List.map (decode_term_from_value st) in
    let ty = decode_type_from_value st ty_v in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> failwith "Term_dag.decode: tm.app requires a type"
    in
    InnerTerm.app ~ty:ty_term f args
  | "tm.builtin" ->
    let vals = read_remaining_values nd in
    let name =
      match vals with
      | D.String s :: _ -> s
      | _ -> failwith "Term_dag.decode: tm.builtin expected string"
    in
    let payload, rest =
      match List.tl vals with
      | D.Null :: rest -> None, rest
      | D.String s :: rest -> Some s, rest
      | rest -> None, rest
    in
    let b = strings_to_builtin name payload in
    let ty_v, term_vals =
      match List.rev rest with
      | ty_v :: arg_vals -> ty_v, List.rev arg_vals
      | [] -> D.Null, []
    in
    let args = List.map (decode_term_from_value st) term_vals in
    let ty = decode_type_from_value st ty_v in
    let ty_term =
      match ty with
      | HasType t -> t
      | NoType -> InnerTerm.tType
    in
    InnerTerm.app_builtin ~ty:ty_term b args
  | _ -> failwith (Printf.sprintf "Term_dag.decode: unknown cmd %S" cmd)

let decode_terms_from_string (data : string) : (int * InnerTerm.t) list =
  let dec = D.create data in
  let st = { dec; terms = Array.make (String.length data + 1) None } in
  D.iter_nodes dec (fun off _cmd _args -> decode_term_at st off |> ignore);
  let result = ref [] in
  Array.iteri
    (fun i -> function
      | Some t -> result := (i, t) :: !result
      | None -> ())
    st.terms;
  List.rev !result

let decode_term (data : string) (off : int) : InnerTerm.t =
  let dec = D.create data in
  let st = { dec; terms = Array.make (String.length data + 1) None } in
  decode_term_at st off

let decode_all (data : string) : (int * InnerTerm.t) list =
  let dec = D.create data in
  let st = { dec; terms = Array.make (String.length data + 1) None } in
  let results = ref [] in
  D.iter_nodes dec (fun off _cmd _args ->
      let t = decode_term_at st off in
      results := (off, t) :: !results);
  List.rev !results

let encode_to_string (f : t -> 'a) : string * 'a =
  let buf = Buffer.create 4096 in
  let out_obj =
    object
      method write (b : bytes) (off : int) (len : int) =
        Buffer.add_subbytes buf b off len
    end
  in
  let enc = E.create ~out:out_obj () in
  let td = create enc in
  let result = f td in
  E.flush enc;
  Buffer.contents buf, result
