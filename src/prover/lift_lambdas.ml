(* open Logtk
module T = Term
module Idx = Fingerprint.Make(T)

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)
  val setup : unit -> unit
end

let lift_lambdas ~index t  =
  let rec aux t =
    let shift_vars = 0 in
    match T.view t with
    | T.Fun _ ->
      let _, body = T.open_fun t in
      let body', defs = aux body in
      let generalization = 
        Idx.retrieve_generalizations (index, 1) (body')
        |> Iter.head in
      match generalization with 
      | Some def ->
        Subst.apply ~shif
    | T.Var _ | T.Const _ -> t, []
    | T.DB _ -> assert false;
    | T.App(hd, l) -> 
      assert(not (T.is_fun hd));
      let l', new_defs = aux_l l in
      T.app hd l', new_defs
    | T.AppBuiltin(b, l) ->
      let l', new_defs = aux_l l in
      T.app hd l', new_defs
    
  and aux_l = function 
    | [] -> ([], [])
    | x :: xs ->
      let x', x_defs = aux x in
      let xs', xs_defs = aux_l xs in
      x' :: xs', x_defs @ xs_defs


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let setup () = ()


end *)