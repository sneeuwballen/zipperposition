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
  match Builtin.is_payload b with
  | Some (Builtin.Int n) -> "Int", Some (Z.to_string n)
  | Some (Builtin.Rat q) -> "Rat", Some (Q.to_string q)
  | Some (Builtin.Real s) -> "Real", Some s
  | Some (Builtin.Pseudo_de_bruijn i) ->
    "Pseudo_de_bruijn", Some (string_of_int i)
  | None ->
    (match Builtin.view b with
    | Some Builtin_gen.Not -> "Not", None
    | Some Builtin_gen.And -> "And", None
    | Some Builtin_gen.Or -> "Or", None
    | Some Builtin_gen.Imply -> "Imply", None
    | Some Builtin_gen.Equiv -> "Equiv", None
    | Some Builtin_gen.Xor -> "Xor", None
    | Some Builtin_gen.Eq -> "Eq", None
    | Some Builtin_gen.Neq -> "Neq", None
    | Some Builtin_gen.HasType -> "HasType", None
    | Some Builtin_gen.True -> "True", None
    | Some Builtin_gen.False -> "False", None
    | Some Builtin_gen.Arrow -> "Arrow", None
    | Some Builtin_gen.Wildcard -> "Wildcard", None
    | Some Builtin_gen.Multiset -> "Multiset", None
    | Some Builtin_gen.TType -> "TType", None
    | Some Builtin_gen.Prop -> "Prop", None
    | Some Builtin_gen.Term -> "Term", None
    | Some Builtin_gen.ForallConst -> "ForallConst", None
    | Some Builtin_gen.ExistsConst -> "ExistsConst", None
    | Some Builtin_gen.ChoiceConst -> "ChoiceConst", None
    | Some Builtin_gen.Grounding -> "Grounding", None
    | Some Builtin_gen.TyInt -> "TyInt", None
    | Some Builtin_gen.TyRat -> "TyRat", None
    | Some Builtin_gen.TyReal -> "TyReal", None
    | Some Builtin_gen.Floor -> "Floor", None
    | Some Builtin_gen.Ceiling -> "Ceiling", None
    | Some Builtin_gen.Truncate -> "Truncate", None
    | Some Builtin_gen.Round -> "Round", None
    | Some Builtin_gen.Prec -> "Prec", None
    | Some Builtin_gen.Succ -> "Succ", None
    | Some Builtin_gen.Sum -> "Sum", None
    | Some Builtin_gen.Difference -> "Difference", None
    | Some Builtin_gen.Uminus -> "Uminus", None
    | Some Builtin_gen.Product -> "Product", None
    | Some Builtin_gen.Quotient -> "Quotient", None
    | Some Builtin_gen.Quotient_e -> "Quotient_e", None
    | Some Builtin_gen.Quotient_t -> "Quotient_t", None
    | Some Builtin_gen.Quotient_f -> "Quotient_f", None
    | Some Builtin_gen.Remainder_e -> "Remainder_e", None
    | Some Builtin_gen.Remainder_t -> "Remainder_t", None
    | Some Builtin_gen.Remainder_f -> "Remainder_f", None
    | Some Builtin_gen.Is_int -> "Is_int", None
    | Some Builtin_gen.Is_rat -> "Is_rat", None
    | Some Builtin_gen.To_int -> "To_int", None
    | Some Builtin_gen.To_rat -> "To_rat", None
    | Some Builtin_gen.Less -> "Less", None
    | Some Builtin_gen.Lesseq -> "Lesseq", None
    | Some Builtin_gen.Greater -> "Greater", None
    | Some Builtin_gen.Greatereq -> "Greatereq", None
    | Some Builtin_gen.Box_opaque -> "Box_opaque", None
    | Some Builtin_gen.BComb -> "BComb", None
    | Some Builtin_gen.CComb -> "CComb", None
    | Some Builtin_gen.IComb -> "IComb", None
    | Some Builtin_gen.KComb -> "KComb", None
    | Some Builtin_gen.SComb -> "SComb", None
    | Some Builtin_gen.Distinct -> "Distinct", None
    | None ->
      failwith
        (Printf.sprintf "Term_dag.builtin_to_strings: unknown builtin %s"
           (Builtin.to_string b)))

let strings_to_builtin (name : string) (payload : string option) : Builtin.t =
  match name, payload with
  | "Int", Some s -> Builtin.make_payload (Builtin.Int (Z.of_string s))
  | "Rat", Some s -> Builtin.make_payload (Builtin.Rat (Q.of_string s))
  | "Real", Some s -> Builtin.make_payload (Builtin.Real s)
  | "Pseudo_de_bruijn", Some s ->
    Builtin.make_payload (Builtin.Pseudo_de_bruijn (int_of_string s))
  | "Not", None -> Builtin.make_view Builtin_gen.Not
  | "And", None -> Builtin.make_view Builtin_gen.And
  | "Or", None -> Builtin.make_view Builtin_gen.Or
  | "Imply", None -> Builtin.make_view Builtin_gen.Imply
  | "Equiv", None -> Builtin.make_view Builtin_gen.Equiv
  | "Xor", None -> Builtin.make_view Builtin_gen.Xor
  | "Eq", None -> Builtin.make_view Builtin_gen.Eq
  | "Neq", None -> Builtin.make_view Builtin_gen.Neq
  | "HasType", None -> Builtin.make_view Builtin_gen.HasType
  | "True", None -> Builtin.make_view Builtin_gen.True
  | "False", None -> Builtin.make_view Builtin_gen.False
  | "Arrow", None -> Builtin.make_view Builtin_gen.Arrow
  | "Wildcard", None -> Builtin.make_view Builtin_gen.Wildcard
  | "Multiset", None -> Builtin.make_view Builtin_gen.Multiset
  | "TType", None -> Builtin.make_view Builtin_gen.TType
  | "Prop", None -> Builtin.make_view Builtin_gen.Prop
  | "Term", None -> Builtin.make_view Builtin_gen.Term
  | "ForallConst", None -> Builtin.make_view Builtin_gen.ForallConst
  | "ExistsConst", None -> Builtin.make_view Builtin_gen.ExistsConst
  | "ChoiceConst", None -> Builtin.make_view Builtin_gen.ChoiceConst
  | "Grounding", None -> Builtin.make_view Builtin_gen.Grounding
  | "TyInt", None -> Builtin.make_view Builtin_gen.TyInt
  | "TyRat", None -> Builtin.make_view Builtin_gen.TyRat
  | "TyReal", None -> Builtin.make_view Builtin_gen.TyReal
  | "Floor", None -> Builtin.make_view Builtin_gen.Floor
  | "Ceiling", None -> Builtin.make_view Builtin_gen.Ceiling
  | "Truncate", None -> Builtin.make_view Builtin_gen.Truncate
  | "Round", None -> Builtin.make_view Builtin_gen.Round
  | "Prec", None -> Builtin.make_view Builtin_gen.Prec
  | "Succ", None -> Builtin.make_view Builtin_gen.Succ
  | "Sum", None -> Builtin.make_view Builtin_gen.Sum
  | "Difference", None -> Builtin.make_view Builtin_gen.Difference
  | "Uminus", None -> Builtin.make_view Builtin_gen.Uminus
  | "Product", None -> Builtin.make_view Builtin_gen.Product
  | "Quotient", None -> Builtin.make_view Builtin_gen.Quotient
  | "Quotient_e", None -> Builtin.make_view Builtin_gen.Quotient_e
  | "Quotient_t", None -> Builtin.make_view Builtin_gen.Quotient_t
  | "Quotient_f", None -> Builtin.make_view Builtin_gen.Quotient_f
  | "Remainder_e", None -> Builtin.make_view Builtin_gen.Remainder_e
  | "Remainder_t", None -> Builtin.make_view Builtin_gen.Remainder_t
  | "Remainder_f", None -> Builtin.make_view Builtin_gen.Remainder_f
  | "Is_int", None -> Builtin.make_view Builtin_gen.Is_int
  | "Is_rat", None -> Builtin.make_view Builtin_gen.Is_rat
  | "To_int", None -> Builtin.make_view Builtin_gen.To_int
  | "To_rat", None -> Builtin.make_view Builtin_gen.To_rat
  | "Less", None -> Builtin.make_view Builtin_gen.Less
  | "Lesseq", None -> Builtin.make_view Builtin_gen.Lesseq
  | "Greater", None -> Builtin.make_view Builtin_gen.Greater
  | "Greatereq", None -> Builtin.make_view Builtin_gen.Greatereq
  | "Box_opaque", None -> Builtin.make_view Builtin_gen.Box_opaque
  | "BComb", None -> Builtin.make_view Builtin_gen.BComb
  | "CComb", None -> Builtin.make_view Builtin_gen.CComb
  | "IComb", None -> Builtin.make_view Builtin_gen.IComb
  | "KComb", None -> Builtin.make_view Builtin_gen.KComb
  | "SComb", None -> Builtin.make_view Builtin_gen.SComb
  | "Distinct", None -> Builtin.make_view Builtin_gen.Distinct
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
