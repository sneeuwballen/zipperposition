
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 State} *)

(** {2 SAT solver Instance} *)

module Bool_lit(X:sig end) = struct
  type view =
    | Select_lit of Lit.t

  type t =
    | 


end

(** {2 State} *)

type t = {
  conf: Flex_state.t;
  ord: Ordering.t;
}

let create ~conf ~ord () =
  let module 
  { conf; ord;
  ord:Ordering.t ->
  unit ->
  t

let conf t = t.conf
let ord t = t.ord
