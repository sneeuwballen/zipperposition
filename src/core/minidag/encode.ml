class type output = object
  method write : bytes -> int -> int -> unit
end

type t = {
  mutable buf: bytes;
  mutable len: int;
  out: output;
  mutable cur_offset: int;  (** How many bytes written into [out] already? *)
}
(** Encoder *)

let buf_size = 1024
let high_watermark = buf_size - 32

let create ~out () : t =
  let out = (out :> output) in
  { out; cur_offset = 0; buf = Bytes.create buf_size; len = 0 }

let[@inline never] flush_ self : unit =
  self.out#write self.buf 0 self.len;
  self.cur_offset <- self.cur_offset + self.len;
  self.len <- 0

let[@inline] abs_offset_ (self : t) : int = self.cur_offset + self.len
let flush self : unit = if self.len > 0 then flush_ self

type node_encoder = t
type offset = int

let maybe_flush self = if self.len >= high_watermark then flush_ self
let[@inline] buf_len self = Bytes.length self.buf

let[@inline never] ensure_slow_ (self : t) n =
  if self.len + n >= high_watermark then flush self;
  let cap = ref (buf_len self) in
  while !cap < self.len + n do
    cap := !cap + (!cap / 2)
  done;
  if !cap > buf_len self then (
    let newbuf = Bytes.create !cap in
    Bytes.blit self.buf 0 newbuf 0 self.len;
    self.buf <- newbuf
  )

(** Ensure at least [n] bytes of free space *)
let ensure_ self n : unit =
  let cap = buf_len self in
  if cap < self.len + n then ensure_slow_ self n;
  assert (self.len + n <= buf_len self)

let write_leading (self : t) ~high ~low =
  if low >= 12 then ensure_ self 9;
  Bytes.set self.buf self.len (Char.unsafe_chr ((high lsl 4) lor low));
  self.len <- self.len + 1

let stop self = write_leading self ~high:0 ~low:0
let null self = write_leading self ~high:1 ~low:0

let bool self b =
  write_leading self ~high:1
    ~low:
      (if b then
         1
       else
         2)

let uint64_ self ~high i =
  assert (i >= 0L);
  if i < 12L then
    write_leading self ~high ~low:(Int64.to_int i)
  else if i < Int64.shift_left 1L 8 then (
    write_leading self ~high ~low:12;
    Bytes.set_int8 self.buf self.len (Int64.to_int i);
    self.len <- self.len + 1
  ) else if i < Int64.shift_left 1L 16 then (
    write_leading self ~high ~low:13;
    Bytes.set_int16_le self.buf self.len (Int64.to_int i);
    self.len <- self.len + 2
  ) else if i < Int64.shift_left 1L 32 then (
    write_leading self ~high ~low:14;
    Bytes.set_int32_le self.buf self.len (Int64.to_int32 i);
    self.len <- self.len + 4
  ) else (
    write_leading self ~high ~low:15;
    Bytes.set_int64_le self.buf self.len i;
    self.len <- self.len + 8
  )

let uint_ self ~high i =
  if i < 12 then
    write_leading self ~high ~low:i
  else
    uint64_ self ~high (Int64.of_int i)

let int64 self i =
  if i = Int64.min_int then
    failwith "Encode.int64: Int64.min_int is not representable"
  else if i < 0L then
    uint64_ self ~high:3 (Int64.neg i)
  else
    uint64_ self ~high:2 i

let int self i =
  if i = min_int then
    uint64_ self ~high:3 Int64.(abs (of_int i))
  else if i < 0 then
    uint_ self ~high:3 (abs i)
  else
    uint_ self ~high:2 i

let float self f =
  write_leading self ~high:4 ~low:15;
  Bytes.set_int64_le self.buf self.len (Int64.bits_of_float f);
  self.len <- self.len + 8

let string_ self ~high s =
  let len = String.length s in
  uint_ self ~high len;
  ensure_ self len;
  Bytes.blit_string s 0 self.buf self.len len;
  self.len <- self.len + len

let string self s = string_ self ~high:5 s
let blob self s = string_ self ~high:6 s

let ref self i =
  assert (i < abs_offset_ self);
  uint_ self ~high:7 i

let write_node (self : t) cmd (f : node_encoder -> unit) : offset =
  let offset = abs_offset_ self in
  string self cmd;
  f self;
  stop self;
  maybe_flush self;
  offset
