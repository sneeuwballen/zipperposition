(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk

let _tmp_dir = ref "/tmp"
let _encode_lams = ref `Keep

exception PolymorphismDetected

module type S = sig
  module Env : Env.S

  (** {5 Registration} *)

  val set_e_bin : string -> unit
  val try_e : Env.C.t Iter.t -> Env.C.t Iter.t -> Proof.proof option

  val setup : unit -> unit
  (** Register rules in the environment *)
end

let _timeout = ref 11
let _e_auto = ref false
let e_bin = ref (None : string option)
let regex_refutation_begin = Str.regexp ".*SZS output start CNFRefutation.*"
let regex_refutation_end = Str.regexp ".*SZS output end CNFRefutation.*"
let reg_thf_clause = Str.regexp "thf(zip_cl_\\([0-9]+\\),.*"

module IntSet = CCSet.Make (CCInt)

module Make (E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module T = Term
  module Combs = Combinators.Make (E)
  module LLift = Lift_lambdas.Make (E)

  let ( ==> ) = Type.( ==> )
  let init_clauses = ref C.ClauseSet.empty
  let initialize () = init_clauses := C.ClauseSet.of_iter Iter.empty

  exception CantEncode of string

  let output_empty_conj ~out =
    Format.fprintf out "thf(conj,conjecture,($false))."

  let rec encode_ty_args_t ~encoded_symbols t =
    let make_new_sym mono_head =
      if not (T.is_ground mono_head) then (
        let err = CCFormat.sprintf "%a has non-ground type argument" T.pp t in
        raise @@ CantEncode err
      );
      let (id, ty), res =
        T.mk_fresh_skolem ~prefix:"ty_enc" [] (T.ty mono_head)
      in
      Env.Ctx.declare id ty;
      res
    in
    let rec aux ~sym_map t =
      if not (Type.is_ground (T.ty t)) then (
        let err =
          CCFormat.sprintf "%a has non-ground type %a" T.pp t Type.pp (T.ty t)
        in
        raise @@ CantEncode err
      );
      match T.view t with
      | T.Const _ | T.Var _ | T.DB _ -> sym_map, t
      | T.Fun (ty, body) ->
        let sym_map', body' = aux ~sym_map body in
        sym_map', T.fun_ ty body'
      | T.AppBuiltin (b, _)
        when (b == Builtin.Eq || b == Builtin.Neq)
             && not (Type.is_prop (T.ty t)) ->
        let err = CCFormat.sprintf "%a is ho bool" T.pp t in
        raise @@ CantEncode err
      (* type erasure for terms E can understand *)
      | T.AppBuiltin (((Builtin.Eq | Builtin.Neq) as b), [ ty; lhs; rhs ])
        when T.is_ground ty ->
        let sym_map, lhs' = aux ~sym_map lhs in
        let sym_map, rhs' = aux ~sym_map rhs in
        sym_map, T.app_builtin ~ty:Type.prop b [ ty; lhs'; rhs' ]
      | T.AppBuiltin
          (((Builtin.ForallConst | Builtin.ExistsConst) as b), [ q; body ])
        when Type.is_ground (T.ty body) ->
        let vars, body = T.open_fun body in
        let sym_map, body' = aux ~sym_map body in
        sym_map, T.app_builtin ~ty:Type.prop b [ q; T.fun_l vars body' ]
      | T.AppBuiltin (_, l) | T.App (_, l) ->
        let hd_mono, args = T.as_app_mono t in
        let sym_map, hd =
          if List.length args != List.length l then (
            match T.Map.get hd_mono sym_map with
            | Some mapped -> sym_map, mapped
            | None ->
              let new_sym = make_new_sym hd_mono in
              T.Map.add hd_mono new_sym sym_map, new_sym
          ) else
            sym_map, hd_mono
        in
        let sym_map, args' =
          List.fold_right
            (fun a (sym_map, as_) ->
              let sym_map, a' = aux ~sym_map a in
              sym_map, a' :: as_)
            args (sym_map, [])
        in
        sym_map, T.app hd args'
    in
    aux ~sym_map:encoded_symbols t

  let encode_ty_args_cl ~encoded_symbols cl =
    let encoded_symbols, lits =
      CCArray.fold_right
        (fun l (encoded_symbols, ls) ->
          match l with
          | Literal.Equation (lhs, rhs, sign) ->
            let encoded_symbols, lhs = encode_ty_args_t ~encoded_symbols lhs in
            let encoded_symbols, rhs = encode_ty_args_t ~encoded_symbols rhs in
            encoded_symbols, Literal.mk_lit lhs rhs sign :: ls
          | _ -> encoded_symbols, l :: ls)
        (C.lits cl) (encoded_symbols, [])
    in
    let rule = Proof.Rule.mk "remove_ty_args" in
    let proof = Proof.Step.inference ~rule [ C.proof_parent cl ] in
    let res =
      if Literals.equal (CCArray.of_list lits) (C.lits cl) then
        cl
      else
        C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lits proof
    in
    encoded_symbols, res

  let output_cl ~out clause =
    let lits_converted = Literals.Conv.to_tst (C.lits clause) in
    match C.distance_to_goal clause with
    | Some d when d = 0 ->
      Format.fprintf out "@[thf(zip_cl_%d,negated_conjecture,@[%a@]).@]@\n"
        (C.id clause) TypedSTerm.TPTP_THF.pp lits_converted
    | _ ->
      Format.fprintf out "@[thf(zip_cl_%d,axiom,@[%a@]).@]@\n" (C.id clause)
        TypedSTerm.TPTP_THF.pp lits_converted

  let output_symdecl ~out sym ty =
    (* distinguished between user defined types and tptp types *)
    Format.fprintf out "@[thf(@['%a_type',type,@[%a@]:@ @[%a@]@]).@]@\n" ID.pp
      sym ID.pp_tstp sym (Type.TPTP.pp_ho ~depth:0) ty

  let output_all ?(already_defined = ID.Set.empty) ~out cl_set =
    let cl_iter = Iter.of_list cl_set in
    let typed_syms =
      C.typed_symbols ~include_types:true cl_iter
      |> Monomorphisation.remove_duplicates ~eq:(fun p1 p2 ->
          ID.equal (fst p1) (fst p2))
    in
    let type_syms, term_syms =
      Monomorphisation.iter_partition
        (fun (_, ty) -> Type.is_tType ty)
        typed_syms
    in
    Iter.iter (fun (sym, ty) -> output_symdecl ~out sym ty) type_syms;
    Iter.iter (fun (sym, ty) -> output_symdecl ~out sym ty) term_syms;
    Iter.iter (output_cl ~out) cl_iter;
    ID.Set.empty

  let set_e_bin path = e_bin := Some path
  let disable_e () = e_bin := None

  let run_e prob_path =
    match !e_bin with
    | Some e_path ->
      let to_ = !_timeout in
      let cmd =
        CCFormat.sprintf
          "timeout %d %s --pos-ext=all --neg-ext=all %s --cpu-limit=%d %s -s -p"
          (to_ + 2) e_path prob_path to_
          (if !_e_auto then
             "--auto"
           else
             "--auto-schedule")
      in
      let process_channel = Unix.open_process_in cmd in
      let refutation_found = ref false in
      let res =
        try
          while not !refutation_found do
            let line = input_line process_channel in
            if Str.string_match regex_refutation_begin line 0 then
              refutation_found := true
          done;
          if !refutation_found then (
            let clause_ids = ref [] in
            try
              while true do
                let line = input_line process_channel in
                flush_all ();
                if Str.string_match reg_thf_clause line 0 then (
                  let id = CCInt.of_string (Str.matched_group 1 line) in
                  clause_ids := CCOpt.get_exn id :: !clause_ids
                ) else if Str.string_match regex_refutation_end line 0 then
                  raise End_of_file
              done;
              Some !clause_ids
            with End_of_file -> Some !clause_ids
          ) else
            None
        with End_of_file -> None
      in
      close_in process_channel;
      res
    | None -> invalid_arg "cannot run E if E binary is not set up"

  module ArgMap = Monomorphisation.ArgMap

  let try_e poly_active_set poly_passive_set =
    let _lambdas_too_deep c =
      let lambda_limit = 6 in
      C.Seq.terms c
      |> Iter.map (fun t -> CCOpt.get_or ~default:0 (Term.lambda_depth t))
      |> Iter.max |> CCOpt.get_or ~default:0
      |> fun lam_depth -> lam_depth > lambda_limit
    in
    let prob_name, prob_channel =
      Filename.open_temp_file ~temp_dir:!_tmp_dir "e_input" ""
    in
    let out = Format.formatter_of_out_channel prob_channel in
    try
      let reconstruct_clause (clause_id, new_lits) original_clause =
        if clause_id = C.id original_clause then (
          (* TODO check that this is indeed a simplification*)
          let rule = Proof.Rule.mk "monomorphisation" in
          let proof_step =
            Proof.Step.simp ~rule [ C.proof_parent original_clause ]
          in
          let new_lits = Array.to_list new_lits in
          let clause_trail = C.trail original_clause in
          let clause_penalty = C.penalty original_clause in
          let res =
            C.create ~penalty:clause_penalty ~trail:clause_trail new_lits
              proof_step
          in
          Some res
        ) else
          None
      in
      let clause_list =
        Iter.to_list (Iter.union ~eq:C.equal poly_active_set poly_passive_set)
      in
      let simple_clause_list =
        List.map (fun cl -> C.id cl, C.lits cl) clause_list
      in
      let monomorphised_clauses =
        Monomorphisation.monomorphise_problem simple_clause_list
      in
      let monomorphised_iter = Iter.of_list monomorphised_clauses in
      let active_set =
        Iter.join ~join_row:reconstruct_clause monomorphised_iter
          poly_active_set
      in
      let passive_set =
        Iter.join ~join_row:reconstruct_clause monomorphised_iter
          poly_passive_set
      in
      let clauses = Iter.append active_set passive_set in
      let ho_clauses = Iter.to_list clauses in
      let _ = output_all ~out ho_clauses in
      close_out prob_channel;
      let cl_set = ho_clauses in
      let start_e_time = Sys.time () in
      let res =
        FileUtil.cp [ prob_name ] FilePath.current_dir;
        match run_e prob_name with
        | Some ids ->
          assert (not (CCList.is_empty ids));
          let clauses =
            List.map (fun id -> List.find (fun cl -> C.id cl = id) cl_set) ids
          in
          let rule = Proof.Rule.mk "eprover" in
          let proof_step =
            Proof.Step.inference ~rule (List.map C.proof_parent clauses)
          in
          let ty = TypedSTerm.prop in
          let proof =
            Proof.S.mk proof_step
              (Proof.Result.of_form
                 (TypedSTerm.app_builtin ~ty Builtin.false_ []))
          in
          Printf.printf "e success\n";
          Some proof
        | _ ->
          Printf.printf "e fail\n";
          None
      in
      Printf.printf "eprover time %f\n" (Sys.time () -. start_e_time);
      res
    with PolymorphismDetected ->
      CCFormat.printf
        "%% Running E stopped because polymorphism was detected @.";
      None

  let setup () = ()
  let () = Signal.once Env.on_start initialize
end

let () =
  Options.add_opts
    [
      ( "--e-encode-lambdas",
        Arg.Symbol
          ( [ "keep"; "drop"; "lift"; "combs" ],
            fun str ->
              match str with
              | "keep" -> _encode_lams := `Keep
              | "drop" -> _encode_lams := `Drop
              | "lift" -> _encode_lams := `Lift
              | "combs" -> _encode_lams := `Combs
              | _ -> failwith "Error: invalide option for --e-encode-lamdas" ),
        " how to treat lambdas when giving problem to E" );
      ( "--tmp-dir",
        Arg.String (fun v -> _tmp_dir := v),
        " scratch directory for running E" );
      "--e-timeout", Arg.Set_int _timeout, " set E prover timeout.";
      ( "--e-auto",
        Arg.Bool (fun v -> _e_auto := v),
        " If set to on eprover will not run in autoschedule, but in auto mode" );
    ]
