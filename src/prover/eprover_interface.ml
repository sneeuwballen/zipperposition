
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

module type S = sig
  module Env : Env.S

  (** {6 Registration} *)


  val set_e_bin : string -> unit
  val try_e : Env.C.t Iter.t -> Env.C.t Iter.t -> unit

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let _timeout = 10 

let e_bin = ref (None : string option)

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx

  let output_empty_conj ~out =
    Format.fprintf out "thf(conj,conjecture,($false))."

  let output_cl ~out clause =
    let lits_converted = Literals.Conv.to_tst (C.lits clause) in
    Format.fprintf out "@[thf(cl_%d,axiom,@[%a@]).@]@\n" (C.id clause) TypedSTerm.TPTP_THF.pp lits_converted

  let output_symdecl ~out sym ty =
    Format.fprintf out "@[thf(@[%a_type,type,@[%a@]:@ @[%a@]@]).@]@\n" 
      ID.pp sym ID.pp sym (Type.TPTP.pp_ho) ty

  let output_all ~out cl_set =
    let cl_iter = Iter.of_list cl_set in
    let syms = ID.Set.to_list @@ C.symbols ~include_types:true cl_iter in
    (* first printing type declarations, and only then the types *)
    CCList.fold_right (fun sym acc -> 
      let ty = Ctx.find_signature_exn sym in
      if Type.is_tType ty then (
        output_symdecl ~out sym ty;
        acc
      ) else (
        assert (not ((Type.Seq.sub ty) |> Iter.exists (Type.is_tType)));
        Iter.cons (sym, ty) acc
      )
    ) syms Iter.empty
    |> Iter.iter (fun (sym, ty) -> output_symdecl ~out sym ty);

    Iter.iter (output_cl ~out) cl_iter;
    output_empty_conj ~out

  let set_e_bin path =
    e_bin := Some path 

  let run_e prob_path =
    match !e_bin with 
    | Some e_path -> 
      (*let cmd = CCFormat.sprintf "%s %s --auto -s -p --cpu-limit %d"  
                  e_path prob_path _timeout in
        Sys.command cmd 
      
        ... test the output and get which clauses have been used in the proof.
       *)
       ()
    | None ->
        invalid_arg "cannot run E if E binary is not set up"


  let try_e active_set passive_set =
    let max_from_set = 500 in

    let take_from_set set = 
      set
      |> Iter.map (fun c -> CCOpt.get_or ~default:c (C.eta_reduce c))
      |> Iter.filter (fun c -> 
          Proof.Step.inferences_perfomed (C.proof_step c) < 5 && 
          C.Seq.terms c |>
          Iter.for_all (fun t -> Term.Seq.subterms t 
                                 |> Iter.for_all (fun st -> not (Term.is_fun st))))
      |> Iter.take max_from_set 
      |> Iter.to_list  in

    let prob_name, prob_channel = Filename.open_temp_file "e_input" "" in
    let out = Format.formatter_of_out_channel prob_channel in
    let cl_set = take_from_set active_set @ (take_from_set passive_set) in
    output_all ~out cl_set;

    Format.printf "[CHECK_E: %s].\n" prob_name; 
    raise (Invalid_argument "done")

    (* let e_name = Filename.temp_file "e_output" "" in *)
    (* let res = run_e e_name in *)
  let setup () =
    ()
end

let () =
  Options.add_opts
    [];