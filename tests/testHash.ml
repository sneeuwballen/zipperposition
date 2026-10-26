open Logtk
module Q = QCheck

(* Reference mirror of [src/hash/hash_stubs.c], in plain Int64 arithmetic:
   xorshift+multiply combiner with the Murmur3 fmix64 finalizer. *)

let ref_seed = 0x9e3779b97f4a7c15L

let ref_combine state chunk =
  let s = Int64.logxor state chunk in
  let s = Int64.logxor s (Int64.shift_right_logical s 32) in
  Int64.mul s 0xc6a4a7935bd1e995L

let ref_fmix64 h =
  let h = Int64.logxor h (Int64.shift_right_logical h 33) in
  let h = Int64.mul h 0xff51afd7ed558ccdL in
  let h = Int64.logxor h (Int64.shift_right_logical h 33) in
  let h = Int64.mul h 0xc4ceb9fe1a85ec53L in
  Int64.logxor h (Int64.shift_right_logical h 33)

(* Max_long in hash_stubs.c is 2^62-1 (resp. 2^30-1 on 32-bit) = [max_int]. *)
let ref_finalize s =
  Int64.to_int (Int64.logand (ref_fmix64 s) (Int64.of_int max_int))

(* hash_stubs.c reads 8-byte chunks with memcpy, so chunk layout is
   native-endian; detect it without assuming it. *)
let ref_little_endian =
  let b = Bytes.make 8 '\000' in
  Bytes.set b 0 '\001';
  Int64.equal Int64.one (Bytes.get_int64_ne b 0)

let ref_chunk s off len =
  let acc = ref 0L in
  for k = 0 to len - 1 do
    let byte = Int64.of_int (Char.code (String.unsafe_get s (off + k))) in
    let shift =
      if ref_little_endian then
        8 * k
      else
        8 * (7 - k)
    in
    acc := Int64.logor !acc (Int64.shift_left byte shift)
  done;
  !acc

let ref_string_hash s =
  let n = String.length s in
  let state = ref ref_seed in
  let i = ref 0 in
  while !i + 8 <= n do
    state := ref_combine !state (ref_chunk s !i 8);
    i := !i + 8
  done;
  if !i < n then state := ref_combine !state (ref_chunk s !i (n - !i));
  ref_finalize !state

let ref_int_hash n = ref_finalize (ref_combine ref_seed (Int64.of_int n))

let ref_int32_hash x =
  ref_finalize
    (ref_combine ref_seed Int64.(logand (of_int32 x) 0x0000_0000_ffff_ffffL))
(* C zero-extends [uint32_t], unlike [Int64.of_int32]. *)

let ref_int64_hash x = ref_finalize (ref_combine ref_seed x)

let ref_char_hash c =
  ref_finalize (ref_combine ref_seed (Int64.of_int (Char.code c)))

let ref_combine2 a b =
  ref_finalize
    (ref_combine (ref_combine ref_seed (Int64.of_int a)) (Int64.of_int b))

let ref_combine3 a b c =
  let s = ref_combine ref_seed (Int64.of_int a) in
  let s = ref_combine s (Int64.of_int b) in
  ref_finalize (ref_combine s (Int64.of_int c))

(* Generators: raw bytes (not just printable ASCII) to exercise high bits. *)

let byte_gen = Q.Gen.(map Char.chr (int_range 0 255))

let gen_string =
  Q.map Bytes.to_string (Q.bytes_size ~gen:byte_gen Q.Gen.(int_range 0 64))
  |> Q.set_print (fun s -> Printf.sprintf "%S" s)

(* Reference agreement *)

let prop_string_ref =
  Q.(
    Test.make ~name:"hash_string_matches_reference" ~count:1000 gen_string
      (fun s -> Hash.string s = ref_string_hash s))

let prop_int_ref =
  Q.(
    Test.make ~name:"hash_int_matches_reference" ~count:1000 int (fun n ->
        Hash.int n = ref_int_hash n))

let prop_primitives_ref =
  Q.(
    Test.make ~name:"hash_char_int32_int64_match_reference" ~count:1000
      (triple char int32 int64) (fun (c, x32, x64) ->
        Hash.char c = ref_char_hash c
        && Hash.int32 x32 = ref_int32_hash x32
        && Hash.int64 x64 = ref_int64_hash x64))

let prop_combine2_ref =
  Q.(
    Test.make ~name:"hash_combine2_matches_reference" ~count:1000 (pair int int)
      (fun (a, b) -> Hash.combine2 a b = ref_combine2 a b))

let prop_combine3_ref =
  Q.(
    Test.make ~name:"hash_combine3_matches_reference" ~count:1000
      (triple int int int) (fun (a, b, c) ->
        Hash.combine3 a b c = ref_combine3 a b c))

let prop_list_ref =
  Q.(
    Test.make ~name:"hash_list_matches_reference" ~count:500
      (list_size (Gen.int_range 0 40) int)
      (fun l ->
        let s =
          List.fold_left
            (fun s x -> ref_combine s (Int64.of_int (Hash.int x)))
            ref_seed l
        in
        Hash.list Hash.int l = ref_finalize s))

(* Stability *)

let prop_stability =
  Q.(
    Test.make ~name:"hash_stable" ~count:500 (pair gen_string int)
      (fun (s, n) ->
        Hash.string s = Hash.string s
        && Hash.int n = Hash.int n
        && Hash.bytes (Bytes.of_string s) = Hash.bytes (Bytes.of_string s)))

(* Different seeds must give different hashes for a fixed input;
   [Hash.combine f s x] mixes [s] as a continuation of the seed. *)

let prop_seed_variation =
  Q.(
    Test.make ~name:"hash_combine_varies_with_seed" ~count:500
      (triple int int int) (fun (s1, s2, x) ->
        s1 = s2 || Hash.combine Hash.int s1 x <> Hash.combine Hash.int s2 x))

(* Avalanche: flipping one input bit changes the output. *)

let prop_avalanche_int =
  Q.(
    Test.make ~name:"hash_int_bit_flip_changes_output" ~count:1000
      (pair int (int_range 0 62))
      (fun (n, k) ->
        let flipped = Int.logxor n (1 lsl k) in
        Hash.int n <> Hash.int flipped))

let prop_avalanche_string =
  Q.(
    Test.make ~name:"hash_string_bit_flip_changes_output" ~count:1000
      (triple
         (bytes_size ~gen:byte_gen (Gen.int_range 1 64))
         (int_range 0 63) (int_range 0 7))
      (fun (b, p, bit) ->
        let b' = Bytes.copy b in
        let pos = p mod Bytes.length b' in
        Bytes.set b' pos
          (Char.chr (Char.code (Bytes.get b' pos) lxor (1 lsl bit)));
        Hash.bytes b <> Hash.bytes b'))

(* Collision sanity: near-injectivity on a sample of strings. *)

let prop_collision_sanity =
  let coll_gen = Q.list_size Q.Gen.(int_range 0 300) gen_string in
  Q.(
    Test.make ~name:"hash_string_few_collisions" ~count:20 coll_gen (fun l ->
        let hs = List.map Hash.string l in
        let distinct = List.sort_uniq compare hs |> List.length in
        distinct * 20 >= List.length hs * 19))

(* Unit tests *)

let pseudo_string len =
  Random.init 42;
  String.init len (fun _ -> Char.chr (Random.int 256))

let test_empty =
  ( "hash.empty input matches reference",
    `Quick,
    fun () ->
      Alcotest.(check int) "empty string" (ref_string_hash "") (Hash.string "");
      Alcotest.(check int)
        "empty bytes" (ref_string_hash "")
        (Hash.bytes (Bytes.create 0)) )

let test_short_lengths =
  ( "hash.string lengths 0..40 match reference",
    `Quick,
    fun () ->
      List.iter
        (fun len ->
          let s = pseudo_string len in
          Alcotest.(check int)
            (Printf.sprintf "len %d" len)
            (ref_string_hash s) (Hash.string s))
        (List.init 41 Fun.id) )

let test_non_negative =
  ( "hash results are non-negative",
    `Quick,
    fun () ->
      let hs =
        [
          Hash.int (-1);
          Hash.int min_int;
          Hash.int max_int;
          Hash.string "foo";
          Hash.string "";
        ]
      in
      List.iter (fun h -> Alcotest.(check bool) ">= 0" true (h >= 0)) hs )

let suite = [ test_empty; test_short_lengths; test_non_negative ]

let props =
  [
    prop_string_ref;
    prop_int_ref;
    prop_primitives_ref;
    prop_combine2_ref;
    prop_combine3_ref;
    prop_list_ref;
    prop_stability;
    prop_seed_variation;
    prop_avalanche_int;
    prop_avalanche_string;
    prop_collision_sanity;
  ]
