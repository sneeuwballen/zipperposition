module T = Term
module Lit = Literal
module Ty = Type
module IT = InnerTerm
module Subst = Subst
module Sc = Scoped
module TermSet = T.Set
module TypeSet = Ty.Set
module ArgMap = Map.Make (ID)
module ClauseArgMap = Map.Make (Int)
module SubstMap = Map.Make (Int)
module PbSubstMap = Map.Make (Int)


(* Note: for ease of notation, variables, functions, comments ... abuse the terms monomorphic and polymorphic 
 * our base language supports polymorphism and formally all the types we handle are polymorphic
 * instead monomorphic should be understood as instantiated (or ground i believe) and polymorphic as not that *)

(* TODO make module *)
(* TODO refactor what we can *)
(* TODO write nice documentation and comments *)
(* TODO if no new type arguments are derived, end the iterations *)
(* TODO clean up interface with the rest of the prover*)
(* TODO tests? *)
(* TODO squash all commits and make the necessary rebase so that this can be added to the main zipperposition branch*)
(* TODO check when Iter.persistent is useful and where Iter.persistent_lazy might be more adapted*)
(* TODO the timeout mechanism is completely independent from the main timeout, this needs to be discussed with
 * someone who knows what they're doing *)

(*invariants:
   - type substitutions replace type variables with monomorphic types
   - the base mono and poly maps are assumed to be able to find all function symbols
     even if they return an empty iter
   - similarly the poly clause map and subst clause maps are assumed to be able to find
     all clause ids
   - each clause has a unique clause id*)

let raise_opt a b f = match a, b with
   | None, None -> None
   | Some x, None -> Some x
   | None, Some y -> Some y
   | Some x, Some y -> Some (f x y)

let ( let+ ) o f = match o with
   | None -> None
   | Some x -> Some (f x)


let ret x = fun _ -> x

type basic_bounds = { relative_bound : float option; absolute_cap : int option; relative_floor : int }


let _loop_count = ref 4
let _mono_ty_args_per_sym = ref { relative_bound = Some 1.0; absolute_cap = Some 50; relative_floor = 7 }
let _poly_ty_args_per_sym = ref { relative_bound = Some 0.0; absolute_cap = Some 10; relative_floor = 1 }
let _mono_ty_args_per_clause = ref { relative_bound = Some 1000000.0; absolute_cap = Some 500; relative_floor = 1000000 }
let _poly_ty_args_per_clause = ref { relative_bound = Some 1000000.0; absolute_cap = Some 500; relative_floor = 1000000 }
let _ty_var_limit = ref 100000
let _monomorphising_subst_per_clause = ref 5
let _new_clauses_relative_bound = ref 2.0
let _substitution_ordering = ref "age"
let _e_max_derived = ref 1000000
let _monomorphisation_timeout = ref 3.0

let begin_time = ref 0.0

(* it would be really nice if a similar function was a part of the iter library*)
(* given an 'a Iter and a 'a -> bool function, splits the iter into a pair using the given function *)
let iter_partition filter iter =
   Iter.fold
     (fun (acc_1, acc_2) elem ->
       if filter elem then (Iter.cons elem acc_1, acc_2) else (acc_1, Iter.cons elem acc_2))
     (Iter.empty, Iter.empty) iter

(* remove duplicates from an iter *)
let remove_duplicates iter ~eq = Iter.map List.hd (Iter.group_by ~eq iter)

(* TODO add this to Type.ml *)

(* Iter.union needs to be provided an equality function when dealing with lists of types *)
(* note that Ty.equal is a physical equality*)
let ty_arg_eq ty_arg1 ty_arg2 =
   assert (List.length ty_arg1 = List.length ty_arg2);
   List.for_all2 Ty.ty_eq ty_arg1 ty_arg2

let lit_is_monomorphic = function
   | Literal.Equation (lt, rt, _) -> T.monomorphic lt && T.monomorphic rt
   | _ -> true

let clause_is_monomorphic (_, lit_arr) = Array.for_all lit_is_monomorphic lit_arr
   
let clause_eq  (_, cl) (_, cl') =
   if Array.length cl = Array.length cl' then Array.for_all2 Literal.equal cl cl' else false

(* the given type does not contain any tType *)
let is_not_meta ty = not (Type.Seq.sub ty |> Iter.exists Type.is_tType)

(* the given type is not meta and requires no type arguments, ie: is well formed*)
let ty_well_formed ty = List.for_all is_not_meta (Ty.expected_args ty) && Ty.expected_ty_vars ty = 0

(* returns the substitution that allows matching a monomorphic type against a type *)
let match_type ~pattern ~mono_type = Unif.Ty.matching_same_scope ~pattern:pattern mono_type ~scope:0

(* returns an (ID, type list) iter that represent the function symbols and their type arguments that 
 * can be extracted from the term *)
let typed_sym term =
   (*get all applications*)
   let all_apps = Iter.filter T.is_app (T.Seq.subterms term) in
   (*get all the function symbols and types at the application level*)
   let get_typed_sym app_term =
      let hd_term = T.head_term_mono app_term in
      let _ , f = T.open_fun hd_term in
      let ty = T.ty hd_term in
         match T.head f with
            | Some id when ty_well_formed ty -> Some (id, T.ty_args f)
            | _ -> None
   in
      Iter.filter_map get_typed_sym all_apps

(* applies a substitution to a type*)
let apply_ty_subst subst ty = Subst.Ty.apply Subst.Renaming.none subst (ty, 0)

(* applies a substitution to a literal *)
let apply_subst_lit subst lit = Literal.apply_subst Subst.Renaming.none subst (lit, 0)

let iter_union ~eq iter_1 iter_2 =
  remove_duplicates (Iter.append iter_1 iter_2) ~eq

(* merges two maps by union of their iters*)
let merge_map_arg_iter (old_ty_args_1, new_ty_args_1) (old_ty_args_2, new_ty_args_2) =
   iter_union ~eq:ty_arg_eq old_ty_args_1 old_ty_args_2, iter_union ~eq:ty_arg_eq new_ty_args_1 new_ty_args_2

(* the union of two substitution iters*)
let iter_subst_union = iter_union ~eq:Subst.equal

(* truncates an iter after len elements *)
let iter_truncate len iter = Iter.take len iter

(* given a list of polymorphic (assumed to be non-monomorphic) types and monomorphic types
 * returns the composition of the substitutions of the matchings of the poly types against the mono types
 * if the substitutions are incompatible or any of the matchings fail, None is returned *)
let type_arg_list_subst type_list_poly type_list_mono =
   let combine curr_subst mono_ty poly_ty =
      match curr_subst with
         | None -> None
         | Some curr_subst -> (
            try
             let new_subst = match_type ~pattern:poly_ty ~mono_type:mono_ty in
                Some (Subst.merge new_subst curr_subst)
            with
               | Subst.InconsistentBinding _ -> None
               | Unif.Fail -> Some curr_subst)
   in
   List.fold_left2 combine (Some Subst.empty) type_list_mono type_list_poly

(* given a basic bound and the size of whatever object we are bounding
 * returns the numbers of elements to keep, if the absolute cap is negative, only the 
 * relative bound is kept, if both are negative, the bounds are ignored *)
let max_nb len bound =
   let rel_cap_opt =
      let+ rel_bound = bound.relative_bound in
      max bound.relative_floor (int_of_float (float_of_int len *. rel_bound))
   in
   raise_opt rel_cap_opt bound.absolute_cap 
      (fun rel_cap abs_cap -> min abs_cap rel_cap)


(* takes a map of function symbols to monomorphic type arguments
 * takes a map of function symbols to polymorphic type arguments
 * returns an iter corresponding to the substitutions generated the type args *)
let derive_type_arg_subst mono_map poly_map =
   (*derives the substitutions from two iters of type arguments*)
   let type_arg_iter_subst mono_type_args_iter poly_type_args_iter =
      let poly_arg_map mono_type_args_iter poly_type_list =
         Iter.filter_map (type_arg_list_subst poly_type_list) mono_type_args_iter
      in
         Iter.flat_map (poly_arg_map mono_type_args_iter) poly_type_args_iter
   in

   let combine fun_sym (old_poly_args, new_poly_args) subst_acc =
      let old_mono_args, new_mono_args = ArgMap.find fun_sym mono_map in

      (* substitutions derived from the new poly type args *)
      (* obtained by matching the new poly type args against both old and new mono type args *)
      let new_poly_subst =
         Iter.persistent_lazy
           (type_arg_iter_subst (remove_duplicates ~eq:ty_arg_eq (Iter.append old_mono_args new_mono_args))
              new_poly_args)
      in

      (* substitutions dervied from the old poly type args and the new mono type args *)
      (* obtained by matching the old poly type args against new mono type args *)
      let old_poly_subst =
         Iter.persistent_lazy (type_arg_iter_subst new_mono_args old_poly_args) in
      
      (* we do not match old poly type args against old mono type args because that will result in substitutions
       * that have already been computed (or ignored depending on the bounds) *)

      iter_subst_union (Iter.append new_poly_subst old_poly_subst) subst_acc
   in

   ArgMap.fold combine poly_map Iter.empty


let print_all_type_args ?(erase_empty = false) fun_sym iter =
   if (not erase_empty) || Iter.length (fst iter) > 0 || Iter.length (snd iter) > 0 then (
     Printf.printf "for this function symbol: %s -- we have the following type arguments  \n(old) :\n"
       (ID.name fun_sym);
     Iter.iter
       (fun ty_args -> Printf.printf "%s\n" (String.concat "; " (List.map Ty.to_string ty_args)))
       (fst iter);
     Printf.printf "(new) :\n";
     Iter.iter
       (fun ty_args -> Printf.printf "%s\n" (String.concat "; " (List.map Ty.to_string ty_args)))
       (snd iter))

(* given a subst map, an iter of substitutions and the current iteration,
   will update the map *)
let update_susbt_map new_subst_iter map iteration_nb =
   let update_map subst = function
      | None -> Some (Iter.singleton (subst, iteration_nb))
      | Some iter -> Some (Iter.cons (subst, iteration_nb) iter)
   in
   let add_single_subst subst_map subst =
         Iter.fold
           (fun curr_map ty_var -> SubstMap.update (HVar.id ty_var) (update_map subst) curr_map)
           subst_map
           (Iter.map fst (Subst.domain subst))
   in
   Iter.fold add_single_subst map new_subst_iter

(* given a single substitution and polymorphic type argument tuples,
 * will generate at most respectively max_new_mono and max_new_poly 
 * monomorphic and non-monomorphic type arguments by application of the substitution
 * to the given type arguments*)
let apply_ty_arg_subst subst poly_ty_args max_new_mono_opt max_new_poly_opt =
   (* type variables instantiated by the substitution *)
   let subst_domain = Iter.map fst (Subst.domain subst) in
   (* Subst.domain returns InnerTerm.t HVar.t, we need Ty.t HVar.t *)
   (* this isn't very pleasant, not sure there is another way of doing things
    * without duplicating Subst.domain *)
   let subst_ty_domain = 
      Iter.map (fun (ty_var: InnerTerm.t HVar.t) -> 
            (HVar.make ~ty:(Ty.of_term_unsafe ty_var.ty) ty_var.id) ) subst_domain
   in

   let ty_var_eq = HVar.equal Ty.ty_eq in

   (* all type variables in a type argument tuple*)
   let ty_vars ty_args =
      remove_duplicates ~eq:ty_var_eq
        (List.fold_left
           (fun acc ty -> Iter.append (Ty.Seq.vars ty) acc)
           Iter.empty ty_args)
   in

   (* iter of (type argument, type variables) pairs, avoids repeating computations later *)
   let poly_ty_args_vars_pair = Iter.map (fun ty_args -> (ty_args, ty_vars ty_args)) poly_ty_args in

   (* a polymorphic type argument tuple is a monomorphic candidate with regards to a given substitution
    * if that substitution instantiates all its type variables *)

   (*splits the poly type args into mono and poly candidates *)
   let mono_candidates, potential_poly_candidates =
      iter_partition (fun (_, ty_vars) -> Iter.subset ~eq:ty_var_eq ty_vars subst_ty_domain) poly_ty_args_vars_pair
   in

   let poly_candidates =
      (* this filters out all polymorphic type argument that would be unaffected by the substitution
       * ie: that have no type variable instantiated by the substitution *)
      Iter.filter
        (fun (_, ty_arg_vars) ->
           Iter.exists (fun ty_var -> Iter.mem ~eq:ty_var_eq ty_var subst_ty_domain) ty_arg_vars)
        potential_poly_candidates
   in

   let max_new_mono = match max_new_mono_opt with
      | None -> Iter.length mono_candidates
      | Some max -> max
   in
   let max_new_poly = match max_new_poly_opt with
      | None -> Iter.length poly_candidates
      | Some max -> max
   in

   (*this function applies the substitution to given monomorphic and polymorphic candidates within bounds*)
   let apply_to_candidates candidates_mono candidates_poly =
      let new_mono = Iter.map (List.map (apply_ty_subst subst)) (Iter.take max_new_mono candidates_mono) in
      let new_poly = Iter.map (List.map (apply_ty_subst subst)) (Iter.take max_new_poly candidates_poly) in
      new_mono, new_poly
   in
   
   apply_to_candidates (Iter.map fst mono_candidates) (Iter.map fst poly_candidates)

(* given a mono and a poly ty arg iter as well as an iter of substitutions and respective mono and poly bounds
 * will return a new mono and a new poly type arg iter within the given bounds *)
let rec generate_ty_args all_subst poly_ty_args max_new_mono_opt max_new_poly_opt =
   let bounds_reached = match max_new_mono_opt, max_new_poly_opt with
      | None, None -> false 
      | Some max_new_mono, None -> max_new_mono <= 0
      | None, Some max_new_poly -> max_new_poly <= 0
      | Some max_new_mono, Some max_new_poly -> max_new_mono <= 0 && max_new_poly <= 0
   in
   match Iter.head all_subst with
      (* no substitutions left *)
      | None -> (Iter.empty, Iter.empty, Iter.empty)
      (* bounds reached *)
      | _ when bounds_reached -> (Iter.empty, Iter.empty, Iter.empty)
      | Some subst ->
         (* apply the current substitution to the type arguments *)
         let new_mono_ty_args, new_poly_ty_args =
            apply_ty_arg_subst subst poly_ty_args max_new_mono_opt max_new_poly_opt in
         (* calculate how many new type arguments we have left we have left *)
         let new_mono_count = 
            let+ max_new_mono = max_new_mono_opt in
            max_new_mono - Iter.length new_mono_ty_args
         in
         let new_poly_count = 
            let+ max_new_poly = max_new_poly_opt in
            max_new_poly - Iter.length new_poly_ty_args
         in

         (* compute remaining type arguments with recursive call*)
         let next_mono_args, next_poly_args, used_substs =
            generate_ty_args (Iter.drop 1 all_subst) poly_ty_args new_mono_count new_poly_count
         in
         (* return all type arguments as well as the substitutions that were used to generate them *)
         Iter.append new_mono_ty_args next_mono_args,
         Iter.append new_poly_ty_args next_poly_args,
         Iter.cons subst used_substs


(* computes the max number of new type arguments for each function symbol *)
(* note that poly_map is not a poly clause map, meaning that this is relative to a clause*)
let max_new_ty_args fun_sym mono_map poly_map =
   let all_mono_ty_args = ArgMap.find fun_sym mono_map in
   let all_poly_ty_args = ArgMap.find fun_sym poly_map in
   let max_mono_bound =
      max_nb
        (Iter.length (fst all_mono_ty_args) + Iter.length (snd all_mono_ty_args))
        !_mono_ty_args_per_sym
   in
   let max_poly_bound =
      max_nb
        (Iter.length (fst all_poly_ty_args) + Iter.length (snd all_poly_ty_args))
        !_poly_ty_args_per_sym
   in
   max_mono_bound, max_poly_bound

(* number of type arguments in a map *)
let clause_total map =
   ArgMap.fold
     (fun _ (old_ty_args, new_ty_args) acc -> acc + Iter.length old_ty_args + Iter.length new_ty_args)
     map 0

(* given the poly map of a clause and the mono map
 * given an iter of substitutions derived from new poly type args
 * given an iter of substitutions derived from old poly type args and new mono type args
 * returns an updated mono and poly map, where the substitutions have been applied (with respect to the bounds)
 * returns an iter of the substitutions that were actually used*)
let apply_subst_map mono_map poly_map new_subst =
   (* number of poly type arguments for this clause*)
   let clause_poly_total = clause_total poly_map in
   let clause_mono_total = clause_total mono_map in

   (* total maximal number of new poly / mono type arguments to be generated for this clause *)
   let mono_clause_max = max_nb clause_mono_total !_mono_ty_args_per_clause in
   let poly_clause_max = max_nb clause_poly_total !_poly_ty_args_per_clause in

   let update_maps fun_sym (old_poly_ty_args, new_poly_ty_args) ((acc_mono_map, acc_poly_map), acc_remaining, acc_used_subst) =
      (* this is the limit per symbol *)
      let per_sym_mono, per_sym_poly = max_new_ty_args fun_sym mono_map poly_map in
      (*this is the limit per clause*)
      let remaining_clause_mono, remaining_clause_poly = acc_remaining in

      (*taking the minimum of both*)
      let remaining_clause_mono = raise_opt remaining_clause_mono per_sym_mono min in
      let remaining_clause_poly = raise_opt remaining_clause_poly per_sym_poly min in

      (* applying substitutions to new poly type args *)
      let new_mono_ty_args, new_poly_ty_args, used_subst =
         generate_ty_args new_subst new_poly_ty_args remaining_clause_mono remaining_clause_poly
      in

      (*calculating the remaining number of type arguments that can be generated *)
      let remaining_local_mono = raise_opt remaining_clause_mono (Some (Iter.length new_mono_ty_args)) ( - ) in
      let remaining_local_poly = raise_opt remaining_clause_poly (Some (Iter.length new_poly_ty_args)) ( - ) in

      (* applying substitutions to old poly type args *)
      let new_mono_ty_args_2, new_poly_ty_args_2, used_subst_2 =
         generate_ty_args new_subst old_poly_ty_args remaining_local_mono remaining_local_poly
      in

      let all_new_mono_ty_args = iter_union ~eq:ty_arg_eq new_mono_ty_args new_mono_ty_args_2 in
      let all_new_poly_ty_args = iter_union ~eq:ty_arg_eq new_poly_ty_args new_poly_ty_args_2 in

      (* keeping track of the substitutions used to generate the type arguments to later generate the clauses *)
      let new_used_substs = Iter.append used_subst used_subst_2 in
      let all_used_substs = iter_subst_union new_used_substs acc_used_subst in

      let new_mono_map = ArgMap.add fun_sym all_new_mono_ty_args acc_mono_map in
      let new_poly_map = ArgMap.add fun_sym all_new_poly_ty_args acc_poly_map in

      let remaining_clause_mono = raise_opt remaining_clause_mono (Some (Iter.length all_new_mono_ty_args)) ( - ) in
      let remaining_clause_poly = raise_opt remaining_clause_poly (Some (Iter.length all_new_poly_ty_args)) ( - ) in

      ((new_mono_map, new_poly_map), (remaining_clause_mono, remaining_clause_poly), all_used_substs)
   in

   (* iterate the previous function through all function symbols *)
   let (new_mono_map, new_poly_map), _, used_substs =
      ArgMap.fold update_maps poly_map
        ((ArgMap.empty, ArgMap.empty), (mono_clause_max, poly_clause_max), Iter.empty)
   in
   new_mono_map, new_poly_map, used_substs

(* given a mono type arg map, a poly type arg map, a subst clause map, the current iteration and bounds
 * will generate new mono and poly ty args within bounds, will also return all substitutions used in 
 * in the process, corresponds to a single monomorphisation step for a given clause *)
let mono_step_clause mono_map poly_map susbt_clause_map curr_iteration =
   (*generate all substitutions from mono and poly type arguments*)
   let new_subst_all = derive_type_arg_subst mono_map poly_map in


   (*apply the substitutions to the poly type arguments*)
   (*split them into the new_mono and new_poly type arguments*)
   let new_mono_map_all, new_poly_map_all, used_substs_iter =
      apply_subst_map mono_map poly_map new_subst_all 
   in

   (* TODO check if eliminating existing type args from the new ones is worth it*)
   let remove_existing new_map curr_map =
      ArgMap.mapi (fun fun_sym new_ty_args -> 
         let old_iter, new_iter = ArgMap.find fun_sym curr_map in
         Iter.diff ~eq:ty_arg_eq new_ty_args (Iter.append old_iter new_iter)) new_map
   in

   (*let new_mono_map = remove_existing new_mono_map_all mono_map in*)
   (*let new_poly_map = remove_existing new_poly_map_all poly_map in*)
   let new_mono_map = new_mono_map_all in
   let new_poly_map = new_poly_map_all in

   (new_mono_map, new_poly_map, update_susbt_map used_substs_iter susbt_clause_map curr_iteration)

(* takes a map from function symbols to iters of monomorphic type arguments
 * takes a map from clause_ids to a map from function symbols to iters of polymorphic type arguments
 * takes a map from clause_ids to a map from type variables to iters of substitutions
 * takes a list of clauses under the form of a (clause_id * literal array), clause_ids are integers
 * returns an updated monomorphic map, polymorphic map and an updated subst map *)
let mono_step clause_list mono_map poly_clause_map subst_map curr_iter =
   (* will execute a single monomorphisation step for each clause *)
   (* note that the newly generated type arguments and substitutions are not re-used during a step,
    * this seperation is important, as it allows the algorithm to be independent of clause order 
    * (at least on a conceptual level, in practice, the bounds make that false) *)
   let process_clause acc (clause_id, literals) =
      let acc_subst, acc_mono_map, acc_poly_clause_map = acc in

      (*assuming it doesn't fail because we previously add all clause ids to the poly_clause_map*)
      let poly_map = ClauseArgMap.find clause_id poly_clause_map in
      let old_clause_subst_map = PbSubstMap.find clause_id subst_map in

      let new_mono_map, new_poly_map, new_clause_subst_map =
         mono_step_clause mono_map poly_map old_clause_subst_map curr_iter 
      in

      let merge_iter _ iter_1 iter_2 = Some (iter_union ~eq:ty_arg_eq iter_1 iter_2) in

      (* not that this way of updating the maps relies on the injectivity of clause ids*)
      let res_mono_map = ArgMap.union merge_iter new_mono_map acc_mono_map in
      let res_poly_clause_map = ClauseArgMap.add clause_id new_poly_map acc_poly_clause_map in
      let res_subst_map = PbSubstMap.add clause_id new_clause_subst_map acc_subst in

      (res_subst_map, res_mono_map, res_poly_clause_map)
   in

   let new_subst_map, new_mono_map, new_poly_clause_map =
      List.fold_left process_clause (subst_map, ArgMap.empty, ClauseArgMap.empty) clause_list
   in

   (* once we have computed the new type arguments, we must update the initial map
    * all old type arguments must be added to the fst of the tuple and the new type
    * arguments to the snd of the tuple*)
   let age_map original_map extra_map =
      let new_args_iter fun_sym =
         match ArgMap.find_opt fun_sym extra_map with Some iter -> iter | None -> Iter.empty
      in
      let iter_age_mapi fun_sym (old_iter, new_iter) =
         (iter_union ~eq:ty_arg_eq old_iter new_iter, new_args_iter fun_sym)
      in
         ArgMap.mapi iter_age_mapi original_map
   in
   let res_mono_map = age_map mono_map new_mono_map in

   (* same thing as the mono map but taking into account the additional clause parameter *)
   let clause_map_age clause_id original_poly_map =
      match ClauseArgMap.find_opt clause_id new_poly_clause_map with
         | None -> age_map original_poly_map ArgMap.empty
         | Some extra_poly_map -> age_map original_poly_map extra_poly_map
   in
   let res_poly_clause_map = ClauseArgMap.mapi clause_map_age poly_clause_map in

   new_subst_map, res_mono_map, res_poly_clause_map

(* given a mono, poly type arg map and a term
 * returns the maps updated with the additional function symbol -> type arguments bindings derived from the term
 * note that all function symbols are added to the maps, even when no corresponding type arguments are found
 * this is to avoid trouble when ArgMap.find is used later *)
(* we assume this function is used only for the initialisation of the maps and that they contain no new type
 * arguments *)
let add_typed_sym mono_map poly_map term =
   let typed_symbols = typed_sym term in
   let type_args_are_mono = List.for_all Ty.is_ground in
   let map_update_fun ty_args add_args = function
      (* since this is used at initialisation, there are no old type arguments*)
      | None when add_args -> Some (Iter.empty, Iter.singleton ty_args)
      | Some (_, curr_iter) when add_args -> Some (Iter.empty, Iter.cons ty_args curr_iter)
      (* this is important to conform to the assumptions, each function symbol must be find-able*)
      | None -> Some (Iter.empty, Iter.empty)
      | Some (_, curr_iter) -> Some (Iter.empty, curr_iter)
   in
   let update_maps (curr_mono_map, curr_poly_map) (ty_sym, ty_args) =
      (* this removes function symbols with no type arguments *)
      (* TODO when replacing the if condition by false on COM025_5, we generate fewer clauses, need to investigate*)
      if ty_args = [] then (curr_mono_map, curr_poly_map)
      else
        let ty_args_mono = type_args_are_mono ty_args in
        let new_mono_map = ArgMap.update ty_sym (map_update_fun ty_args ty_args_mono) curr_mono_map in
        let new_poly_map = ArgMap.update ty_sym (map_update_fun ty_args (not ty_args_mono)) curr_poly_map in
           (new_mono_map, new_poly_map)
   in
   Iter.fold update_maps (mono_map, poly_map) typed_symbols

(* initialises substitution map for a clause, allows later use of find for any type variable in a clause *)
let init_single_subst_map literals =
   let all_vars = Literals.Seq.vars literals in
   let all_ty_vars = Iter.filter (fun var -> Type.equal (HVar.ty var) Type.tType) all_vars in
   Iter.fold (fun acc ty_var -> SubstMap.add (HVar.id ty_var) Iter.empty acc) SubstMap.empty all_ty_vars

(* will initialise the maps with the function symbol -> type arguments bindings derived from the clauses *)
let map_initialisation_step (mono_map, clause_poly_map, pb_subst_map) (clause_id, literals) =
   (* collects all terms into an iter*)
   let clause_terms_iter = Iter.persistent_lazy (Literals.Seq.terms literals) in

   (* updates the mono and poly maps with the relevant function symbol -> type argument bindings *)
   let update_maps (curr_mono_map, curr_poly_map) term = add_typed_sym curr_mono_map curr_poly_map term in
   let new_subst_map = PbSubstMap.add clause_id (init_single_subst_map literals) pb_subst_map in
   let new_mono_map, new_poly_map = Iter.fold update_maps (mono_map, ArgMap.empty) clause_terms_iter in

   (* removes duplicates from a map *)
   let remove_duplicate_init map =
      ArgMap.map (fun (_, iter) -> (Iter.empty, Iter.persistent (remove_duplicates ~eq:ty_arg_eq iter))) map in
   let new_poly_map_unique = remove_duplicate_init new_poly_map in
   let new_mono_map_unique = remove_duplicate_init new_mono_map in

   let new_clause_poly_map =
      match ClauseArgMap.find_opt clause_id clause_poly_map with
         | None -> ClauseArgMap.add clause_id new_poly_map_unique clause_poly_map
         (*should not happen if each clause has a unique id*)
         | Some other_poly_map ->
            Printf.printf "Error: different clauses have the same id\n";
            assert false;
   in
   new_mono_map_unique, new_clause_poly_map, new_subst_map


(* takes a substitution map, an iter of type variables to instantiate and the maximum number of
 * authorised new substitutions*)
(* returns an iter of substitutions that instantiate all type variables *)
let generate_monomorphising_subst subst_map ty_var_iter max_new_subst =
   (* messy type casting due to Subst.domain *)
   let subst_ty_domain subst = 
      Iter.map (fun (ty_var: InnerTerm.t HVar.t) -> 
            (HVar.make ~ty:(Ty.of_term_unsafe ty_var.ty) ty_var.id)) (Iter.map fst (Subst.domain subst))
   in
   let ty_var_eq = HVar.equal Ty.ty_eq in

   let remaining_ty_vars ty_var_iter subst =
      Iter.diff ~eq:ty_var_eq ty_var_iter (subst_ty_domain subst)
   in
   (* this function explores the tree of substitutions that will instantiate the type variables *)
   (* subst_acc is the substitution corresponding to the current node, it instantiates all the type variables
    * that have been removed from vars_to_instantiate *)
   (* acc_iter is the iter of all previously explored leaves ie: substitutions that instantiate all 
    * relevant type variables *)
   (* acc_count is the number of substitutions we have left *)
   let rec create_subst (subst_acc, acc_iter, acc_count) vars_to_instantiate =
      if acc_count <= 0 then (acc_iter, acc_count)
      else
        (*look at the next variable to instantiate *)
        match Iter.head vars_to_instantiate with
            (* no variable left to instantiate subst_acc is a monormophising substitution
             * if it has already been added, we ignore it, this can happen if for instance
             * a substitution is the composition of two other substitutions in the map *)
           | None ->
                 if Iter.mem ~eq:Subst.equal subst_acc acc_iter then (acc_iter, acc_count)
                 else (Iter.cons subst_acc acc_iter, acc_count - 1)
           | Some ty_var ->
              (* all substitutions which instantiate the current type variable *)
              let candidate_subst = Iter.map fst (SubstMap.find (HVar.id ty_var) subst_map) in
              (* process a single substitution*)
              let process_subst process_acc_iter acc_count next_subst =
                 match Subst.merge subst_acc next_subst with
                    (* next_subst was not compatible with subst_acc *)
                    | exception _ -> (Iter.empty, acc_count)
                    (* the substitutions were compatible, they are merged and we move deeper in the tree *)
                    | merged_subst ->
                       let new_vars_to_instantiate = remaining_ty_vars vars_to_instantiate merged_subst in
                       create_subst (merged_subst, process_acc_iter, acc_count) new_vars_to_instantiate
              in
              (* wrapper around the function to make it foldable*)
              let process_fold (fold_acc_iter, acc_count) next_subst =
                 if acc_count <= 0 then (fold_acc_iter, acc_count)
                 else process_subst fold_acc_iter acc_count next_subst
              in
              (* continue tree exploration, note that this is a depth first search, this 
               * is because the tree is explosive and we want to stop as soon as we reached the limit *)
              Iter.fold process_fold (acc_iter, acc_count) candidate_subst
   in
   let monomorphising_subst_iter = create_subst (Subst.empty, Iter.empty, max_new_subst) ty_var_iter |> fst in
   Iter.persistent_lazy monomorphising_subst_iter

(* counts the number of type arguments in a map *)
let count_arg_map arg_map =
   ArgMap.fold (fun _ (old_iter, new_iter) acc -> Iter.length old_iter + Iter.length new_iter + acc) arg_map 0

(* counts number of type arguments in a map, counting old and new type arguments seperately *)
let count_arg_map_split arg_map =
   ArgMap.fold
     (fun _ (old_iter, new_iter) (old_acc, new_acc) ->
       (Iter.length old_iter + old_acc, Iter.length new_iter + new_acc))
     arg_map (0, 0)

(* counts the number of type arguments in a clause type argument map *)
let count_clause_arg_map clause_arg_map =
   ClauseArgMap.fold (fun _ arg_map acc -> count_arg_map arg_map + acc) clause_arg_map 0

(* all type variables in a clause *)
let clause_ty_vars lit_arr =
   let var_eq = HVar.equal (fun _ _ -> true) in
   let all_vars =
      Iter.persistent_lazy
         (remove_duplicates ~eq:var_eq
           (Iter.flat_map
              (fun lit -> Literal.Seq.vars lit )
              (Iter.of_array lit_arr)))
   in
      Iter.filter (fun var -> Type.equal (HVar.ty var) Type.tType) all_vars

(* computes a map with all the substitutions that were generated during a given iteration *)
let subst_map_filter_age subst_map (iteration_count:int) =
   SubstMap.map
     (fun subst_iter -> Iter.filter (fun (subst, age) -> age = iteration_count) subst_iter)
     subst_map

(* we simulate what would happen if the substitutions generated during an iteration were only combined
 * with other substitutions from the same iterations *)
let age_mono_subst subst_map vars_to_instantiate mono_subst_limit =
   let rec iter_mono_subst iteration new_subst_limit =
      let iter_subst_map = subst_map_filter_age subst_map iteration in
      if iteration < 0 then Iter.empty
      else
        let prev_subst = iter_mono_subst (iteration - 1) new_subst_limit in
        if Iter.length prev_subst >= new_subst_limit then Iter.take new_subst_limit prev_subst
        else
         let curr_subst =
            generate_monomorphising_subst iter_subst_map vars_to_instantiate !_monomorphising_subst_per_clause in
         remove_duplicates ~eq:Subst.equal (Iter.append prev_subst curr_subst)
   in
   iter_mono_subst !_loop_count mono_subst_limit

let age_merge_mono_subst subst_map vars_to_instantiate mono_subst_limit =
   let sort_iter = Iter.sort_uniq ~cmp:(fun (_, age_1) (_, age_2) -> compare age_1 age_2) in
   let sorted_subst_map = SubstMap.map sort_iter subst_map in
   let mono_subst = generate_monomorphising_subst sorted_subst_map vars_to_instantiate mono_subst_limit in
   remove_duplicates ~eq:Subst.equal mono_subst


(* shuffle all substitutions, we multiply the mono subst limit by loop_count because it is a limit
 * per iteration *)
let random_mono_subst subst_map vars_to_instantiate mono_subst_limit =
   let random_subst_map = SubstMap.map Iter.shuffle subst_map in
   generate_monomorphising_subst random_subst_map vars_to_instantiate mono_subst_limit 


(* different methods for instantiating a clause from a substitution map within bounds*)
let instantiate_clause subst_map (clause_id, lit_arr) new_clauses_remaining =
   let vars_to_instantiate = clause_ty_vars lit_arr in
   (* different methods of generating monomorphising substitutions *)
   let mono_subst = match !_substitution_ordering with
      | "age" -> 
            age_mono_subst subst_map vars_to_instantiate new_clauses_remaining
      | "random" -> 
            random_mono_subst subst_map vars_to_instantiate (new_clauses_remaining * !_loop_count)
      | "arbitrary" -> 
            generate_monomorphising_subst subst_map vars_to_instantiate (new_clauses_remaining * !_loop_count)
      | "age-merge" -> 
            age_merge_mono_subst subst_map vars_to_instantiate (new_clauses_remaining * !_loop_count)
      | _ ->
            Printf.printf "Error: incorrect substitution map ordering option\n"; assert false;
   in

   let apply_subst subst lit_arr = Array.map (apply_subst_lit subst) lit_arr in
   let new_lits_iter = Iter.map (fun subst -> apply_subst subst lit_arr) (Iter.persistent_lazy mono_subst) in
   Iter.map (fun lit_arr -> (clause_id, lit_arr)) new_lits_iter




(* takes a list of (clause_id, literal array) pairs
 * takes an integer to limit the numbers of iterations
 * returns an updated list of clauses *)
let monomorphise_problem_base clause_list =

   (*List.iter (fun (_, cl) -> Printf.printf "clause: %s\n" (Literals.to_string cl)) clause_list;*)

   (*List.iter (fun (_, cl) -> Literals.Seq.terms cl (fun term -> (Term.to_string term) ^ " : " ^ (Type.to_string (Term.ty term)) |> Printf.printf "%s\n") ) clause_list;*)

   (*Printf.printf "list length: %i\n" (List.length clause_list);*)
   (* initialisation *)
   (* create initial maps *)
   let init_mono_map, init_clause_poly_map, init_subst_map =
      List.fold_left map_initialisation_step (ArgMap.empty, ClauseArgMap.empty, PbSubstMap.empty) clause_list
   in


   (*Printf.printf "initialisation done at: %f\n" (Sys.time () -. !begin_time );*)

   (* all non-monomorphic clauses *)
   let poly_clause_list = List.filter (fun cl -> clause_is_monomorphic cl |> not) clause_list in
   (* all monomorphic clauses *)
   let mono_clause_list = List.filter clause_is_monomorphic clause_list in

   (* monomorphisation loop *)
   let rec monomorphisation_loop curr_count mono_map poly_clause_map subst_map =
      if curr_count <= 0 then (mono_map, poly_clause_map, subst_map)
      else(
        (*let mono_count = count_arg_map mono_map in*)
        (*let poly_count = count_clause_arg_map poly_clause_map in*)
        (*Printf.printf "mono count: %i, poly count: %i\n" mono_count poly_count;*)
        let new_subst, new_mono_map, new_poly_clause_map =
           mono_step poly_clause_list mono_map poly_clause_map subst_map curr_count in
        monomorphisation_loop (curr_count - 1) new_mono_map new_poly_clause_map new_subst)
   in
   (*Printf.printf "initialisation done at: %f\n" (Sys.time () -. !begin_time );*)

   (* we don't really care about the type arguments, the substitutions are the important part*)
   let final_mono, final_non_mono, subst_map_res =
      let init_mono_map =
         ArgMap.map (fun (_, init_iter) -> (Iter.persistent Iter.empty, Iter.persistent init_iter)) init_mono_map in
      monomorphisation_loop !_loop_count init_mono_map init_clause_poly_map init_subst_map
   in

   (*let print_all_ty_args map =
      ArgMap.iter (fun fun_sym (old_iter, new_iter) ->
         print_all_type_args fun_sym (old_iter, new_iter)) map
   in*)
   (*print_all_ty_args final_mono;*)
   (*Printf.printf "now for the non monomorphic \n";*)
   (*ClauseArgMap.iter (fun clause_id map -> print_all_ty_args map) final_non_mono;*)
   (*Printf.printf "iterations done at: %f\n" (Sys.time () -. !begin_time );*)


   (* once we have the substitutions, we want to use them to instantiate the polymorphic clauses *)
   let new_clauses_fold (acc_cl, acc_remaining) (clause_id, lit_arr) =
      if acc_remaining <= 0 then (acc_cl, acc_remaining)
      else
        let new_clauses =
           instantiate_clause (PbSubstMap.find clause_id subst_map_res) (clause_id, lit_arr) acc_remaining in
        let new_remaining = acc_remaining - Iter.length new_clauses in
           (Iter.append acc_cl (iter_truncate acc_remaining new_clauses), new_remaining)
   in
   let total_clause_limit =
      let relative_max = 
         int_of_float (!_new_clauses_relative_bound *. float_of_int (List.length clause_list)) in
      max !_e_max_derived relative_max in

   let new_clauses =  List.fold_left new_clauses_fold (Iter.empty, total_clause_limit) poly_clause_list
      |> fst |> remove_duplicates ~eq:clause_eq in


   let final_clause_list = Iter.to_list new_clauses @ mono_clause_list in

   (* we want to have; monomorphisation time, number of initial poly and mono clauses, number of output clauses*)
   (*let print_end_info =
      let subst_count pb_subst_map =
         PbSubstMap.fold (fun _ subst_map acc ->
             acc + SubstMap.fold (fun _ subst_iter acc -> acc + Iter.length subst_iter) subst_map 0) pb_subst_map 0
      in
      let new_clause_count = Iter.length new_clauses in
      let all_new_subst = subst_count subst_map_res in
      let mono_time = Sys.time () -. !begin_time in
         Printf.printf "monomorphisation time: %f\n" mono_time;
         Printf.printf "new clauses: %i\n" new_clause_count;
         Printf.printf "all final substitutions: %i\n" all_new_subst;
   in*)

   (*print_end_info;*)

   final_clause_list

let mangle_lit str_ty_list str_term_list lit =
   let open Lit in
   match lit with
      | True -> str_ty_list, str_term_list, mk_tauto
      | False -> str_ty_list, str_term_list, mk_absurd
      | Equation (lt, rt, b) ->
            let str_ty_list, str_term_list, new_lt = Term.mangle_term str_ty_list str_term_list lt in
            let str_ty_list, str_term_list, new_rt = Term.mangle_term str_ty_list str_term_list rt in
            str_ty_list, str_term_list, (mk_lit new_lt new_rt b)

let mangle_clause str_ty_list str_term_list clause =
   let clause_id, lit_arr = clause in
   let term_list = Array.to_list lit_arr in
   let str_ty_list, str_term_list, new_term_list = Term.fold_left_map2 mangle_lit str_ty_list str_term_list term_list in
   let new_clause = (clause_id, Array.of_list new_term_list) in
   str_ty_list, str_term_list, new_clause

(* copied (and slightly modified) from https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431 *)
(* lets us run the monomorphisation procedure with a timeout *)
exception Out_of_time
let run_with_time_limit limit f =
  (* create a Unix timer timer *)
  let _ = Unix.setitimer Unix.ITIMER_REAL Unix.{it_value = limit; it_interval = 0.01 } in
  (* The Unix.timer works by sending a Sys.sigalrm, so in order to use it,
     we catch it and raise the Out_of_time exception. *)
  let () =
    Sys.set_signal Sys.sigalrm (
      Sys.Signal_handle (fun _ ->
          raise Out_of_time)
      ) in
  Fun.protect f ~finally:(fun () ->
     let _ = Unix.setitimer Unix.ITIMER_REAL Unix.{it_value = 0.; it_interval = 0. } in () )

let monomorphise_problem clause_list =
   let curr_time = Sys.time () in
   try run_with_time_limit !_monomorphisation_timeout (fun () -> 
      let monomorphised_clauses = monomorphise_problem_base clause_list in
      let _, _, res = Term.fold_left_map2 mangle_clause [] [] monomorphised_clauses in
   (*List.iter (fun (_, cl) -> Literals.Seq.terms cl (fun term -> (Term.to_string term) ^ " : " ^ (Type.to_string (Term.ty term)) |> Printf.printf "%s\n") ) res;*)
      (*let res = monomorphise_problem_base clause_list in*)
      Printf.printf "real monomorphisation time: %f\n" (Sys.time () -. curr_time);
      res
   )
   with Out_of_time -> Printf.printf "Monomorphisation timed out\n"; []


let rec convert_type ty =
   let open Ty in
   let args = expected_args ty in
   let ret = returns ty in
      if args != [] then List.map convert_type args ==> convert_type ret
      else
        match view ty with
           | Builtin _ -> ty 
           | _ -> Ty.const (ID.make (Ty.mangle ty))

let float_to_opt str =
   try 
      let res = float_of_string str in
      if res < 0. then None else Some res
   with _ -> None

let int_to_opt str =
   try 
      let res = int_of_string str in
      if res < 0 then None else Some res
   with _ -> None

let () =
   Options.add_opts
     [
        ("--sym-mono-ty-args", Arg.String (fun s ->
           match String.split_on_char ',' s with
            | [cap;mult;floor] ->
               _mono_ty_args_per_sym := { relative_bound = float_to_opt mult; 
                                          absolute_cap = int_to_opt cap; 
                                          relative_floor = int_of_string floor }
            | _ -> failwith "invalid mono ty args options"),
            " parameters for controlling the number of new monomorphic type argument for each symbol per clause per iteration");
        ("--sym-poly-ty-args", Arg.String (fun s ->
           match String.split_on_char ',' s with
            | [cap;mult;floor] ->
               _poly_ty_args_per_sym := { relative_bound = float_to_opt mult; 
                                          absolute_cap = int_to_opt cap; 
                                          relative_floor = int_of_string floor }
            | _ -> failwith "invalid poly ty args options"),
            " parameters for controlling the number of new polymorphic type argument for each symbol per clause per iteration");
        ("--clause-mono-ty-args", Arg.String (fun s ->
           match String.split_on_char ',' s with
            | [cap;mult;floor] ->
               _mono_ty_args_per_clause := { relative_bound = float_to_opt mult; 
                                             absolute_cap = int_to_opt cap; 
                                             relative_floor = int_of_string floor }
            | _ -> failwith "invalid mono ty args options"),
            " parameters for controlling the number of new monomorphic type argument per clause per iteration");
        ("--clause-poly-ty-args", Arg.String (fun s ->
           match String.split_on_char ',' s with
            | [cap;mult;floor] ->
               _poly_ty_args_per_clause := { relative_bound = float_to_opt mult;
                                             absolute_cap = int_to_opt cap; 
                                             relative_floor = int_of_string floor }
            | _ -> failwith "invalid poly ty args options"),
            " parameters for controlling the number of new polymorphic type argument per clause per iteration");
       ("--mono-loop", Arg.Int (( := ) _loop_count), " number of iterations of the monomorphisation algorithm");
       ( "--new-clauses-multiplier",
         Arg.Float (( := ) _new_clauses_relative_bound),
         " maximum number of new clauses the monomorphisation algorithm will generate, expressed as a factor of \
          the initial number of clauses destined for monomorphisation" );
       ( "--monomorphising-subst-per-clause",
         Arg.Int (( := ) _monomorphising_subst_per_clause),
         " maximum number of instantiating substitutions that can be generated by monomorphisation algorithm \
          per clause per iteration" );
      ( "--substitution-ordering",
         Arg.String (fun s ->
             match s with
                | "random" -> _substitution_ordering := "random"
                | "age" -> _substitution_ordering := "age"
                | "arbitrary" -> _substitution_ordering := "arbitrary"
                | "age-merge" -> _substitution_ordering := "age-merge"
                | _ -> failwith "invalid substitution ordering"),
         " substitution ordering used at the clause generation phase of the monomorphisation algorithm" );
       ( "--e-max-derived",
         (* TODO if this is greater than _max_derived, print warning that we are creating superfluous clauses *)
         Arg.Set_int _e_max_derived,
         " set the limit of clauses that are derived by Zipperposition and given to E" );
      ("--monomorphisation-timeout",
         Arg.Float (fun v -> _monomorphisation_timeout := v),
         " timeout for monomorphisation process");
     ]
