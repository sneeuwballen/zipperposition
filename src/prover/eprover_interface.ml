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
let _max_derived = ref 100000000
let _only_ho_steps = ref true
let _sort_by_weight_only = ref false
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
  (*let initialize () = init_clauses := C.ClauseSet.of_iter (Env.get_passive ())*)
  let initialize () = init_clauses := C.ClauseSet.of_iter Iter.empty

  exception CantEncode of string

  let output_empty_conj ~out = Format.fprintf out "thf(conj,conjecture,($false))."

  let rec encode_ty_args_t ~encoded_symbols t =
     let make_new_sym mono_head =
        (if not (T.is_ground mono_head) then
           let err = CCFormat.sprintf "%a has non-ground type argument" T.pp t in
              raise @@ CantEncode err);
        let (id, ty), res = T.mk_fresh_skolem ~prefix:"ty_enc" [] (T.ty mono_head) in
           Env.Ctx.declare id ty;
           res
     in

     let rec aux ~sym_map t =
        (if not (Type.is_ground (T.ty t)) then
           let err = CCFormat.sprintf "%a has non-ground type %a" T.pp t Type.pp (T.ty t) in
              raise @@ CantEncode err);
        match T.view t with
           (*| T.Const id ->
                 (match Type.view (T.ty t) with
                     | App(id, []) ->
                        Printf.printf "type id: %i, type string: %s\n" (ID.id id) (Type.to_string (T.ty t));
                        Env.Ctx.declare id (Type.tType)
                     | _ -> ()
                  );
                 Printf.printf "const id: %s, const type: %s\n" (ID.to_string id) (Type.to_string (T.ty t));
                 Env.Ctx.declare id (T.ty t);
                 (sym_map, t)*)
           | T.Const _ | T.Var _ | T.DB _ -> (sym_map, t)
           | T.Fun (ty, body) ->
              let sym_map', body' = aux ~sym_map body in
                 (sym_map', T.fun_ ty body')
           | T.AppBuiltin (b, _) when (b == Builtin.Eq || b == Builtin.Neq) && not (Type.is_prop (T.ty t)) ->
              let err = CCFormat.sprintf "%a is ho bool" T.pp t in
                 raise @@ CantEncode err
           (* type erasure for terms E can understand *)
           | T.AppBuiltin (((Builtin.Eq | Builtin.Neq) as b), [ ty; lhs; rhs ]) when T.is_ground ty ->
              let sym_map, lhs' = aux ~sym_map lhs in
              let sym_map, rhs' = aux ~sym_map rhs in
                 (sym_map, T.app_builtin ~ty:Type.prop b [ ty; lhs'; rhs' ])
           | T.AppBuiltin (((Builtin.ForallConst | Builtin.ExistsConst) as b), [ q; body ])
             when Type.is_ground (T.ty body) ->
              let vars, body = T.open_fun body in
              let sym_map, body' = aux ~sym_map body in
                 (sym_map, T.app_builtin ~ty:Type.prop b [ q; T.fun_l vars body' ])
           | T.AppBuiltin (_, l) | T.App (_, l) ->
              let hd_mono, args = T.as_app_mono t in
              let sym_map, hd =
                 if List.length args != List.length l then
                   match T.Map.get hd_mono sym_map with
                      | Some mapped -> (sym_map, mapped)
                      | None ->
                         let new_sym = make_new_sym hd_mono in
                            (T.Map.add hd_mono new_sym sym_map, new_sym)
                 else (sym_map, hd_mono)
              in
              let sym_map, args' =
                 List.fold_right
                   (fun a (sym_map, as_) ->
                     let sym_map, a' = aux ~sym_map a in
                        (sym_map, a' :: as_))
                   args (sym_map, [])
              in
                 (sym_map, T.app hd args')
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
                     (encoded_symbols, Literal.mk_lit lhs rhs sign :: ls)
               | _ -> (encoded_symbols, l :: ls))
          (C.lits cl) (encoded_symbols, [])
     in
     let rule = Proof.Rule.mk "remove_ty_args" in
     let proof = Proof.Step.inference ~rule [ C.proof_parent cl ] in
     let res =
        if Literals.equal (CCArray.of_list lits) (C.lits cl) then cl
        else C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lits proof
     in
        (encoded_symbols, res)

  let output_cl ~out clause =
     (*Printf.printf "output clause: %s\n" (C.to_string clause);*)
     let lits_converted = Literals.Conv.to_tst (C.lits clause) in
     (*Printf.printf "output converted lit: %s\n" (TypedSTerm.to_string lits_converted);*)
        (*Printf.printf "converted literals: %s\n" (TypedSTerm.to_string lits_converted);*)
        (*Format.fprintf out "%% %d:\n" (C.proof_depth clause);*)
        (*let orig_cl_str = CCFormat.sprintf "%% @[%a@]@." C.pp_tstp clause in*)
        (*let commented = CCString.replace ~which:`All ~sub:"\n" ~by:"\n% " orig_cl_str in*)
        (*Format.fprintf out "%% orig:@.@[%s@]@." commented;*)
        match C.distance_to_goal clause with
           | Some d when d = 0 ->
              Format.fprintf out "@[thf(zip_cl_%d,negated_conjecture,@[%a@]).@]@\n" (C.id clause)
                TypedSTerm.TPTP_THF.pp lits_converted
           | _ ->
              Format.fprintf out "@[thf(zip_cl_%d,axiom,@[%a@]).@]@\n" (C.id clause)
                TypedSTerm.TPTP_THF.pp lits_converted

  let output_symdecl ~out sym ty =
     (* distinguished between user defined types and tptp types *)
     (*let usr_sym = ID.make (ID.name sym ^ "_u") in*)
        Format.fprintf out "@[thf(@['%a_type',type,@[%a@]:@ @[%a@]@]).@]@\n" ID.pp sym ID.pp_tstp sym
          (Type.TPTP.pp_ho ~depth:0) ty

  let output_all ?(already_defined = ID.Set.empty) ~out cl_set =
      let cl_iter = Iter.of_list cl_set in
      (*let syms =
        C.symbols ~include_types:true cl_iter |> (fun syms -> ID.Set.diff syms already_defined) |> ID.Set.to_list |> List.rev
      in*)
      (*CCList.iter (fun sym -> Printf.printf "symbol: %s\n" (ID.to_string sym)) syms ;*)
      let typed_syms = C.typed_symbols ~include_types:true cl_iter |> Monomorphisation.remove_duplicates ~eq:(fun p1 p2 -> ID.equal (fst p1) (fst p2)) in
      let type_syms, term_syms = Monomorphisation.iter_partition (fun (_, ty) -> Type.is_tType ty) typed_syms in
      (*Iter.iter (fun (id, _) -> Printf.printf "typed symbol: %s\n" (ID.to_string id)) typed_syms;*)
      Iter.iter (fun (sym, ty) -> output_symdecl ~out sym ty) type_syms;
      Iter.iter (fun (sym, ty) -> output_symdecl ~out sym ty) term_syms;
      
      (*CCList.fold_right
       (fun sym _ ->
         let ty = Ctx.find_signature_exn sym in
         Printf.printf "symbol: %s, type: %s\n" (ID.to_string sym) (Type.to_string ty);
            output_symdecl ~out sym ty)
       syms ();*)

        Iter.iter (output_cl ~out) cl_iter;
        (*if ID.Set.is_empty already_defined then output_empty_conj ~out;*)
        (*ID.Set.of_list syms*)
        ID.Set.empty

  let set_e_bin path = e_bin := Some path
  let disable_e () = e_bin := None

  let run_e prob_path =
     Printf.printf "path: %s\n" prob_path;
     match !e_bin with
        | Some e_path ->
           let to_ = !_timeout in
           let cmd =
              CCFormat.sprintf "timeout %d %s --pos-ext=all --neg-ext=all %s --cpu-limit=%d %s -s -p" (to_ + 2)
                e_path prob_path to_
                (if !_e_auto then "--auto" else "--auto-schedule")
           in
           let process_channel = Unix.open_process_in cmd in
           let refutation_found = ref false in
           let res =
              try
                while not !refutation_found do
                  let line = input_line process_channel in
                     (*Printf.printf "%s\n" line;*)
                     if Str.string_match regex_refutation_begin line 0 then refutation_found := true
                  (*Printf.printf "we got that bool: %b!\n" !refutation_found;*)
                done;
                if !refutation_found then
                  let clause_ids = ref [] in
                     try
                       while true do
                         let line = input_line process_channel in
                            flush_all ();
                            if Str.string_match reg_thf_clause line 0 then
                              let id = CCInt.of_string (Str.matched_group 1 line) in
                                 clause_ids := CCOpt.get_exn id :: !clause_ids
                            else if Str.string_match regex_refutation_end line 0 then raise End_of_file
                       done;
                       Some !clause_ids
                     with End_of_file -> Some !clause_ids
                else None
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

     let convert_clauses ~converter ~encoded_symbols iter =
        let converted =
           let res = Iter.map (fun c -> 
            CCOpt.get_or ~default:c (C.eta_reduce c)) iter |> Iter.flat_map_l converter in
           res
        in

        (*Iter.iter (fun cl -> Printf.printf "\nBefore conversion: %s\n" (C.to_string cl)) converted;*)
        let encoded, encoded_symbols =
           Iter.fold
             (fun (acc, encoded_symbols) cl ->
               try
                 let encoded_symbols, cl' = encode_ty_args_cl ~encoded_symbols cl in
                    (cl' :: acc, encoded_symbols)
               with CantEncode reason ->
                 Util.debugf 5 "cannot encode(%s):@.@[%a@]@." (fun k -> k reason C.pp cl);
                 (acc, encoded_symbols))
             ([], encoded_symbols) converted
        in
           (encoded_symbols, encoded)
     in

     (*let take_initial ~converter () =
        (*Printf.printf "TAKE INITIAL: %i\n" (C.ClauseSet.cardinal !init_clauses);*)
        (*List.iter (fun cl -> Printf.printf "\ninitial clause: %s" (C.to_string cl)) (C.ClauseSet.to_list !init_clauses);*)
        let module CS = C.ClauseSet in
        CS.filter (fun c -> not (lambdas_too_deep c)) !init_clauses
        |> CS.to_iter
        |> convert_clauses ~converter ~encoded_symbols:T.Map.empty
     in*)

     let take_ho_clauses ~converter ~encoded_symbols clauses =
        (* TODO we ignored proof depth conditions because the clauses we have are the ones generated by the monomorphisation
         * and therefore it doesn't matter that they = 0, the subsequent sorting and taking !max_derived makes
         * removing those with depth 5 or greater somewhat redundant because we already eliminate clauses with high
         * proof depth that we don't want, need to double check that this doesn't cause any issues*)
        (* in fact sorting by proof depth may not make any sense considering that the newly generated clauses are
         * not derived through means in which the notion of proof depth is relevant (as far as i understand) *)
        clauses
        (*|> Iter.sort ~cmp:(fun c1 c2 -> CCInt.compare (C.ho_weight c1) (C.ho_weight c2))*)
        (*|> Iter.take !_max_derived*)
        |> convert_clauses ~converter ~encoded_symbols
     in

     let prob_name, prob_channel = Filename.open_temp_file ~temp_dir:!_tmp_dir "e_input" "" in
     let out = Format.formatter_of_out_channel prob_channel in

     try
       let converter =
          match !_encode_lams with
             | `Keep -> fun c -> [ c ]
             | `Drop -> fun c -> if Iter.exists T.has_lambda (C.Seq.terms c) then [] else [ c ]
             | `Combs -> fun c -> ([ Combs.force_conv_lams c ] :> C.t list)
             | _ ->
                fun c ->
                   let lifted = LLift.lift_lambdas c in
                      if CCList.is_empty lifted then [ c ] else lifted
       in


       (*List.iter (fun cl -> Printf.printf "\nWe have a clause: %s" (C.to_string cl)) ((E.C.ClauseSet.to_list !init_clauses) @ (Iter.to_list poly_active_set) @ (Iter.to_list poly_passive_set));*)
       (*List.iter (fun cl -> Printf.printf "\nWe have a clause: %s" (C.to_string cl)) (poly_initial @ (Iter.to_list poly_active_set) @ (Iter.to_list poly_passive_set));*)
       (*List.iter (fun cl -> Printf.printf "\nWe have a clause: %s" (C.to_string cl)) ([] @ (Iter.to_list poly_active_set) @ (Iter.to_list poly_passive_set));*)
       let reconstruct_clause (clause_id, new_lits) original_clause =
          if clause_id = C.id original_clause then
            (* TODO check that this is indeed a simplification*)
            let rule = Proof.Rule.mk "monomorphisation" in
            let proof_step = Proof.Step.simp ~rule [ C.proof_parent original_clause ] in
            let new_lits = Array.to_list new_lits in
            (* Have no idea what I'm doing here *)
            let clause_trail = C.trail original_clause in
            let clause_penalty = C.penalty original_clause in
            let res = C.create ~penalty:clause_penalty ~trail:clause_trail new_lits proof_step in
               (*Printf.printf "\n%s\n" (C.to_string res);*)
               Some res
          else None
       in

       let clause_list =
          Iter.to_list
            (*(Iter.union ~eq:C.equal (Iter.of_list poly_initial)*)
            (Iter.union ~eq:C.equal poly_active_set poly_passive_set)
       in
       (*List.iter (fun cl -> Printf.printf "\noriginal clause: %s" (C.to_string cl)) clause_list;*)
       let simple_clause_list = List.map (fun cl -> (C.id cl, C.lits cl)) clause_list in
       
       let monomorphised_clauses = Monomorphisation.monomorphise_problem simple_clause_list in

       (*Printf.printf "EProver intf clause nb %i\n" (List.length monomorphised_clauses);*)
       let monomorphised_iter = Iter.of_list monomorphised_clauses in

       let active_set = Iter.join ~join_row:reconstruct_clause monomorphised_iter poly_active_set in
       let passive_set = Iter.join ~join_row:reconstruct_clause monomorphised_iter poly_passive_set in

       (*let encoded_symbols, poly_initial = take_initial ~converter () in*)

       (*let initial =
          Iter.to_list (Iter.join ~join_row:reconstruct_clause monomorphised_iter (Iter.of_list poly_initial))
       in*)


       let clauses = (Iter.append active_set passive_set) in
       (*let encoded_symbols, clauses = Iter.fold fold_encode_clauses (T.Map.empty, Iter.empty) clauses in*)
       (*let _, ho_clauses = take_ho_clauses  ~encoded_symbols:T.Map.empty ~converter clauses in*)
       let ho_clauses = Iter.to_list clauses in
       (*List.iter (fun lit -> Printf.printf "ho_clause lit: %s\n" (C.to_string lit)) ho_clauses;*)

       (*Printf.printf "new initital clause nb: %i\n" (List.length poly_initial);*)
       (*Printf.printf "initial initital clause nb: %i\n" (List.length poly_initial);*)
       (*let mangle_clause cl =
             let mangled_lits = Array.map (fun lit -> Literal.map Term.mangle_term lit) (C.lits cl) in
             reconstruct_clause (C.id cl, mangled_lits) cl
         in
         let ho_clauses = List.filter_map mangle_clause ho_clauses in*)

       (*Printf.printf "ho clause nb: %i\n" (List.length ho_clauses);*)
       (*List.iter (fun cl -> Printf.printf "this clause has taken its toll, wohoh i know she said [Polymorphism Detected] too many times before wohohoh: %s\n" (C.to_string_tstp cl)) ho_clauses;*)
       let _ = output_all ~out ho_clauses in
          (*Format.fprintf out "%% -- PASSIVE -- \n";*)
          (*ignore (output_all ~already_defined ~out ho_clauses);*)
          close_out prob_channel;
          let cl_set = ho_clauses in

          let start_e_time = Sys.time() in
          let res =
             FileUtil.cp [prob_name] FilePath.current_dir;
             Printf.printf "monomorphised problem name %s\n" prob_name;
             assert false;
             match run_e prob_name with
                | Some ids ->
                   assert (not (CCList.is_empty ids));

                   let clauses = List.map (fun id -> List.find (fun cl -> C.id cl = id) cl_set) ids in
                   let rule = Proof.Rule.mk "eprover" in
                   let proof_step = Proof.Step.inference ~rule (List.map C.proof_parent clauses) in
                   let ty = TypedSTerm.prop in
                   let proof =
                      Proof.S.mk proof_step (Proof.Result.of_form (TypedSTerm.app_builtin ~ty Builtin.false_ []))
                   in
                      Printf.printf "e success\n";
                      Some proof
                | _ ->
                   Printf.printf "e fail\n";
                   None
          in
             (* Sys.remove prob_name; *)
             Printf.printf "eprover time %f\n" (Sys.time() -. start_e_time);
             res
     with PolymorphismDetected ->
       CCFormat.printf "%% Running E stopped because polymorphism was detected @.";
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
                   | _ -> assert false ),
         " how to treat lambdas when giving problem to E" );
       ("--tmp-dir", Arg.String (fun v -> _tmp_dir := v), " scratch directory for running E");
       ("--e-timeout", Arg.Set_int _timeout, " set E prover timeout.");
       ( "--e-sort-by-weight-only",
         Arg.Bool (( := ) _sort_by_weight_only),
         " order the clauses only by the weight, not by the proof depth." );
       (* TODO this option seems to not be implemented, check with someone whether that is indeed the case *)
       ("--e-only-ho-steps", Arg.Bool (( := ) _only_ho_steps), " translate only HO proof steps to E");
       ( "--e-auto",
         Arg.Bool (fun v -> _e_auto := v),
         " If set to on eprover will not run in autoschedule, but in auto mode" );
      ( "--max-derived",
         Arg.Int ((:=) _max_derived ),
         " maximum number of clauses to send to E" );
     ]
