type t = { str: string }
(** Decoder *)

let create str : t = { str }
let raw_string (self : t) : string = self.str

type offset = int

type node_decoder = {
  dec: t;
  mutable off: offset;
}

type value =
  | Stop  (** No other value left *)
  | Null
  | Bool of bool
  | Int64 of int64
  | Float of float
  | String of string
  | Blob of string
  | Ref of int

exception Fail of string * offset

let fail off msg = raise (Fail (msg, off))
let failf off msg = Printf.ksprintf (fail off) msg

let[@inline] read_byte_ (self : node_decoder) : int =
  let c = String.get self.dec.str self.off in
  self.off <- self.off + 1;
  Char.code c

let[@inline] read_leading_ (self : node_decoder) =
  let c = read_byte_ self in
  c lsr 4, c land 0x0f

let read_uint64 self ~low =
  match low with
  | _ when low < 12 -> Int64.of_int low
  | 12 -> read_byte_ self |> Int64.of_int
  | 13 ->
    let n = String.get_int16_le self.dec.str self.off in
    self.off <- self.off + 2;
    Int64.of_int (n land 0xFFFF)
  | 14 ->
    let n = String.get_int32_le self.dec.str self.off in
    self.off <- self.off + 4;
    Int64.logand (Int64.of_int32 n) 0xFFFFFFFFL
  | 15 ->
    let n = String.get_int64_le self.dec.str self.off in
    self.off <- self.off + 8;
    n
  | _ -> assert false

let string_ self ~low : string =
  let len = read_uint64 self ~low |> Int64.to_int in
  let s = String.sub self.dec.str self.off len in
  self.off <- self.off + len;
  s

let read (self : node_decoder) : value =
  let off_start = self.off in
  let high, low = read_leading_ self in
  match high with
  | 0 -> Stop
  | 1 ->
    (match low with
    | 0 -> Null
    | 1 -> Bool true
    | 2 -> Bool false
    | n -> failf off_start "invalid special: %d" n)
  | 2 -> Int64 (read_uint64 self ~low)
  | 3 -> Int64 (Int64.neg (read_uint64 self ~low))
  | 4 -> Float (Int64.float_of_bits (read_uint64 self ~low))
  | 5 -> String (string_ self ~low)
  | 6 -> Blob (string_ self ~low)
  | 7 -> Ref (read_uint64 self ~low |> Int64.to_int)
  | _ -> failf off_start "invalid high: %d" high

let read_int (self : node_decoder) : int =
  match read self with
  | Int64 i -> Int64.to_int i
  | _ -> fail self.off "expected int"

let read_string (self : node_decoder) : string =
  match read self with
  | String s -> s
  | _ -> fail self.off "expected string"

let read_blob (self : node_decoder) : string =
  match read self with
  | Blob s -> s
  | _ -> fail self.off "expected blob"

let read_ref (self : node_decoder) : offset =
  match read self with
  | Ref off -> off
  | Null -> -1
  | _ -> fail self.off "expected ref"

let read_all_refs (self : node_decoder) : offset list =
  let rec collect acc =
    match read self with
    | Ref off -> collect (off :: acc)
    | Stop -> List.rev acc
    | _ -> fail self.off "expected ref"
  in
  collect []

let read_node (self : t) (off : offset) f =
  let dec = { dec = self; off } in
  match read dec with
  | String s -> f dec s
  | _ -> fail off "expected node to start with a string"

let iter_nodes (self : t) (f : offset -> string -> value list -> unit) : unit =
  let total_len = String.length self.str in
  let rec go off =
    if off < total_len then (
      let dec = { dec = self; off } in
      match read dec with
      | String cmd ->
        let rec collect acc =
          match read dec with
          | Stop -> List.rev acc
          | v -> collect (v :: acc)
        in
        let args = collect [] in
        f off cmd args;
        go dec.off
      | _ -> fail off "expected string at start of node"
    )
  in
  go 0
