open Why3
open Stdlib

type key = int
(* The key type, with which we identify nodes in the Why3 VC tree *)

module Keygen : sig
   (* A small module that provides a trivial key generator for the session tree
    *)
   val keygen : ?parent:'a -> unit -> key
end = struct

   let count = ref 0

   let keygen ?parent () =
      ignore (parent);
      incr count;
      !count
end


type goal = key Session.goal
(* the type of goals; a goal is an elementary VC *)

type subp =
  { subp_goal : goal;
    subp_entity : Gnat_expl.subp_entity
  }
(* This type stores the goal which corresponds to a subprogram (the whole
   correctness formula for a subp), together with the entity information which
   describes it *)

type objective = Gnat_expl.check
(* an objective is identified by its explanation, which contains the source
   location and the kind of the check *)

type status =
   | Proved
   | Not_Proved
   | Work_Left
   | Counter_Example

(* the session variable, it is initialized later on *)
let my_session : key Session.env_session option ref = ref None
let get_session () =
   match !my_session with
   | Some s -> s
   | None -> assert false

module GoalHash = struct
   (* module to provide hashing and fast equality on goals *)
   type t = goal
   let equal a b = a.Session.goal_key = b.Session.goal_key
   let hash a = a.Session.goal_key
end

module GoalCmp = struct
   (* module to provide comparison goals *)
  type t = goal
  let compare a b = Pervasives.compare a.Session.goal_key b.Session.goal_key
end

module GoalMap = Hashtbl.Make (GoalHash)
(* module to provide a mutable map on goals *)

module GoalSet : sig
   (* module to provide mutable sets on goals *)
   type t
   val empty : unit -> t
   val is_empty : t -> bool
   val add : t -> goal -> unit
   val remove : t -> goal -> unit
   val choose : t -> goal
   val mem    : t -> goal -> bool
   val count  : t -> int
   val reset : t -> unit
   val iter   : (goal -> unit) -> t -> unit
end =
struct
   (* We use an ordered set instead of a hashed set here so that we have
      predictable order of iteration. *)

   module S = Set.Make(GoalCmp)
   type t = S.t ref

   let empty () = ref S.empty
   let is_empty t = S.is_empty !t
   let add t x =
     t := S.add x !t
   let remove t x =
     t := S.remove x !t
   let mem t x =
     S.mem x !t
   let count t =
     S.cardinal !t
   let reset t =
     t := S.empty
   let iter f t =
     S.iter f !t

   exception Found of goal
   let choose t =
      try
         iter (fun k -> raise (Found k)) t;
         raise Not_found
      with Found k -> k
end

type objective_rec =
   { to_be_scheduled    : GoalSet.t;
     (* when a goal is scheduled, it is removed from this set *)
     to_be_proved       : GoalSet.t;
     (* when a goal is proved (or unproved), it is removed from this set *)
     toplevel           : GoalSet.t;
     (* the set of top-level goals, that is not obtained by transformation.
      * They are "entry points" of the objective into the Why3 session *)
     mutable not_proved : bool;
   (* when a goal is not proved, the objective is marked "not proved" by
    * setting this boolean to "true" *)
     mutable counter_example : bool;
   (* when a goal is not proved and a counterexample for the goal should
    * be got, the objective is marked "counterexample" by setting this
    * boolean to "true" *)
   }
(* an objective consists of to be scheduled and to be proved goals *)

let empty_objective () =
   { to_be_scheduled = GoalSet.empty ();
     to_be_proved    = GoalSet.empty ();
     toplevel        = GoalSet.empty ();
     not_proved      = false;
     counter_example = false
   }

(* The state of the module consists of these mutable structures *)
let explmap : objective_rec Gnat_expl.HCheck.t = Gnat_expl.HCheck.create 17
(* maps proof objectives to goals *)

let goalmap : Gnat_expl.check GoalMap.t = GoalMap.create 17
(* maps goals to their objectives *)

let total_nb_goals : int ref = ref 0
let nb_objectives : int ref = ref 0
let nb_goals_done : int ref = ref 0

let not_interesting : GoalSet.t = GoalSet.empty ()

let clear () =
   Gnat_expl.HCheck.clear explmap;
   GoalMap.clear goalmap;
   GoalSet.reset not_interesting;
   total_nb_goals := 0;
   nb_objectives := 0;
   nb_goals_done  := 0

let find e =
   try Gnat_expl.HCheck.find explmap e
   with Not_found ->
      let r = empty_objective () in
      Gnat_expl.HCheck.add explmap e r;
      incr nb_objectives;
      r

let add_to_objective ~toplevel ex go =
  (* add a goal to an objective.
   * A goal can be "top-level", that is a direct goal coming from WP, or not
   * top-level, that is obtained by transformation. *)
   let filter =
      match Gnat_config.limit_line with
      | Some (Gnat_config.Limit_Line l) ->
         Gnat_loc.equal_line l (Gnat_expl.get_loc ex)
      | Some (Gnat_config.Limit_Check c) ->
         (c.Gnat_expl.reason = Gnat_expl.get_reason ex)
         && (Gnat_loc.equal_orig_loc c.Gnat_expl.sloc (Gnat_expl.get_loc ex))
      | None -> true
   in
   if filter then begin
      incr total_nb_goals;
      GoalMap.add goalmap go ex;
      let obj = find ex in
      GoalSet.add obj.to_be_scheduled go;
      GoalSet.add obj.to_be_proved go;
      if toplevel then GoalSet.add obj.toplevel go;
   end

let get_objective goal = GoalMap.find goalmap goal

let add_clone derive goal =
   let obj = get_objective derive in
   add_to_objective ~toplevel:false obj goal

let set_not_interesting x = GoalSet.add not_interesting x
let is_not_interesting x = GoalSet.mem not_interesting x
let is_interesting x = not (is_not_interesting x)

let next objective =
   (* this lookup should always succeed, otherwise it would mean we have a
      corrupt database *)
   let obj_rec = Gnat_expl.HCheck.find explmap objective in
   let rec build acc n =
     if n = 0 then acc
     else try
        (* the [choose] can fail however, in that case we want to return
           the goals found up to now *)
        let goal = GoalSet.choose obj_rec.to_be_scheduled in
        GoalSet.remove obj_rec.to_be_scheduled goal;
        build (goal :: acc) (n-1)
     with Not_found ->
        acc
   in
   build [] Gnat_config.parallel

let strategy =
  match Gnat_config.proof_mode with
  | Gnat_config.Per_Path -> ["path_split"; Gnat_split_conj.split_conj_name]
  | Gnat_config.Per_Check -> ["split_goal_wp_conj"]
  | _ ->
      ["split_goal_wp_conj";
       Gnat_split_disj.split_disj_name]

let parent_transform_name goal =
   match goal.Session.goal_parent with
   | Session.Parent_transf t -> t.Session.transf_name
   | _ -> assert false

let rev_strategy = List.rev strategy

let last_transform = List.hd rev_strategy

let first_transform = List.hd strategy

let next_transform =
  let h = Hashtbl.create 17 in
  let rec fill before l =
    match l with
    | [] -> ()
    | x::rest ->
        Hashtbl.add h before x;
        fill x rest
  in
  let _ =
    match strategy with
    | [] -> assert false
    | head::tail -> fill head tail
  in
  (fun trans -> Hashtbl.find h trans)

exception Found_Trans of string

let get_first_transform_of_goal g =
  (* return a random transformation that has been applied to the goal. If only
     gnatprove was run on this file, there is only one transformation. *)
  try
    Session.PHstr.iter (fun k _ -> raise (Found_Trans k))
    g.Session.goal_transformations;
    assert false
  with Found_Trans s -> s

let find_next_transformation goal =
  (* the "then" branch corresponds to the "normal" case where only gnatprove
     was applied *)
  if Session.PHstr.is_empty goal.Session.goal_transformations then
      try next_transform (parent_transform_name goal)
      with Not_found ->
        Gnat_util.abort_with_message ~internal:true
          "unknown transformation found"
  else
    (* in the other case, we just apply the transformation that's there *)
    get_first_transform_of_goal goal

let is_full_split_goal goal =
   (* check whether other transformations should be applied to the goal. If the
      transformation is part of the strategy, we check if it is the last one.
      Otherwise, the goal is fully split if there are no transformations
      applied to it (that we could follow) *)
  if not (Session.PHstr.is_empty goal.Session.goal_transformations) then false
  else
    let s = parent_transform_name goal in
    not (List.mem s strategy) || s = last_transform

let has_already_been_applied trans goal =
   (* check whether the goal has already been split by the given
      transformation *)
   Session.PHstr.mem goal.Session.goal_transformations trans

let further_split goal =
   (* check which was the last transformation applied to the goal and
      apply the next one on the list. Note that this may have already been done
      in a previous session, in which case we simply return the underlying
      goals. If it hasn't been done yet, we apply the transformation. If not
      more than one new goal is obtained this way, we move to the next
      transformation in the strategy list. If that still doesn't help, we
      return the empty list. *)
   let rec split trans =
     if has_already_been_applied trans goal then
         let transf =
            Session.PHstr.find goal.Session.goal_transformations trans in
           transf.Session.transf_goals
     else
         let transf =
            Session.add_registered_transformation
              ~keygen:Keygen.keygen
              (get_session ())
              trans
              goal in
         let new_goals = transf.Session.transf_goals in
         if List.length new_goals > 1 then begin
           new_goals
         end else begin
            Session.remove_transformation transf;
            try
              let trans' = next_transform trans in
              split trans'
            with Not_found -> []
         end
   in
   split (find_next_transformation goal)


let has_been_tried_by g prover =
   (* Check whether the goal has been tried already *)
  let prover = prover.Session.prover_config.Whyconf.prover in
   try
      Session.goal_iter (fun child ->
         match child with
         | Session.Proof_attempt pa ->
               (* only count non-obsolete proof attempts with identical
                  options *)
               if not pa.Session.proof_obsolete &&
               pa.Session.proof_prover = prover &&
               pa.Session.proof_limit =
                Gnat_config.limit ~prover:prover.Whyconf.prover_name then
                  raise Exit
         | _ -> ()) g;
      false
   with Exit -> true

let all_provers_tried g =
  List.for_all (has_been_tried_by g) Gnat_config.provers

let unproved_vc_continue obj obj_rec =
  (* This function checks whether proof should continue even though we have an
     unproved VC. This function raises Exit when:
     * lazy mode is on (default)
     * no more VCs left
     otherwise it returns obj, Work_Left *)
  obj_rec.not_proved <- true;
  if Gnat_config.lazy_ then raise Exit;
  if GoalSet.is_empty obj_rec.to_be_proved then raise Exit;
  obj, Work_Left

let register_result goal result =
   let obj = get_objective goal in
   let obj_rec = Gnat_expl.HCheck.find explmap obj in
   (* We first remove the goal from the list of goals to be tried. It may be
    * put back later, see below *)
   GoalSet.remove obj_rec.to_be_proved goal;
   if obj_rec.counter_example then begin
     (* The prover run was scheduled just to get counterexample *)
     obj, Not_Proved
   end else begin
     incr nb_goals_done;
     if result then begin
     (* goal has been proved, we only need to store that info *)
       if not (GoalSet.is_empty obj_rec.to_be_proved) then obj, Work_Left
       else if obj_rec.not_proved then obj, Not_Proved else obj, Proved
     end else begin try
         (* the goal was not proved. *)
         (* We first check whether another prover may apply *)
         if Gnat_config.manual_prover = None &&
            not (all_provers_tried goal) then begin
           (* put the goal back to be scheduled and proved *)
           GoalSet.add obj_rec.to_be_scheduled goal;
           GoalSet.add obj_rec.to_be_proved goal;
           obj, Work_Left
         end else begin
           (* This particular goal has been tried with all provers. But maybe
              we can split/apply transformations. *)
           if is_full_split_goal goal then unproved_vc_continue obj obj_rec
           else
           let new_goals = further_split goal in
           if new_goals = [] then unproved_vc_continue obj obj_rec
           else begin
             (* if we are here, it means we have simplified the goal. We add the
                new goals to the set of goals to be proved/scheduled. *)
             List.iter (add_clone goal) new_goals;
             obj, Work_Left
           end
         end
       with Exit ->
         (* if we cannot simplify, the objective has been disproved *)
         let n = GoalSet.count obj_rec.to_be_scheduled in
         GoalSet.reset obj_rec.to_be_scheduled;
         nb_goals_done := !nb_goals_done + n;

         if Gnat_config.counterexamples then begin
           (* The goal will be scheduled to get a counterexample *)
           obj_rec.not_proved <- true;
           obj_rec.counter_example <- true;
           GoalSet.add obj_rec.to_be_proved goal;
           (* The goal will be scheduled manually in Gnat_main.handle_result
               so it is not put to the obj_rec.to_be_scheduled *)

             obj, Counter_Example
         end else obj, Not_Proved
   end
   end

let objective_status obj =
   let obj_rec = Gnat_expl.HCheck.find explmap obj in
   if obj_rec.counter_example then Counter_Example
   else if GoalSet.is_empty obj_rec.to_be_proved then
     if obj_rec.not_proved then Not_Proved else Proved
   else if GoalSet.is_empty obj_rec.to_be_scheduled then
      Not_Proved
   else
      Work_Left


let iter f =
   let obj = Gnat_expl.HCheck.fold (fun k _ acc -> k :: acc) explmap [] in
   List.iter f obj

let get_num_goals () =
   !total_nb_goals

let get_num_goals_done () =
   !nb_goals_done

let has_file session =
   (* Check whether the session has a file associated with it. Sessions without
      files can happen in strange cases (gnatwhy3 crashes in the wrong moment)
      *)
   try
      Session.session_iter (fun any ->
         match any with
         | Session.File _ -> raise Exit
         | _ -> ()) session.Session.session;
      false
   with Exit -> true


let iter_main_goals fu =
   (* Main goals are at the following point in the theory:
        session -> file -> theory -> subgoal
                                     *here*

      They correspond to program functions (one big goal for each program)
   *)
   Session.session_iter (fun any ->
      match any with
      | Session.File f ->
            Session.file_iter (fun any ->
               match any with
               | Session.Theory t ->
                     Session.theory_iter (fun any ->
                        match any with
                        | Session.Goal g ->
                              fu g
                        | _ -> ()) t
               | _ -> ()) f
      | _ -> ()) (get_session ()).Session.session

let iter_leafs goal f =
      Session.goal_iter (fun any ->
         match any with
         | Session.Transf t
            when t.Session.transf_name = first_transform ->
               Session.transf_iter (fun any ->
                  match any with
                  | Session.Goal g ->
                      f g
                  | _ -> ()) t
         | _ -> ()) goal

let iter_leaf_goals subp f = iter_leafs subp.subp_goal f

exception Prover_Found of Session.loaded_prover

let find_obsolete_valid_proof g =
  (* if there is an obsolete valid proof of goal g with prover p, such that p
     is among the selected provers, return [Some p], otherwise return None *)
  try
    Session.goal_iter (fun child ->
      match child with
      | Session.Proof_attempt
        ({ Session.proof_obsolete = true;
          proof_state = Session.Done
            { Call_provers.pr_answer = Call_provers.Valid }}
        as pa) ->
          begin
            match Gnat_config.is_selected_prover pa.Session.proof_prover with
            | Some p -> raise (Prover_Found p)
            | None -> ()
          end
      | _ -> ()) g;
    None
  with Prover_Found p -> Some p

let find_best_untried_prover g =
  (* return the manual prover, if there is one. Otherwise, if an obsolete valid
     proof exists, try that prover first. Otherwise, just try the first not yet
     tried prover. *)
  match Gnat_config.manual_prover with
  | Some p -> p
  | None ->
      match find_obsolete_valid_proof g with
      | Some p -> p
      | None ->
          List.find (fun p -> not (has_been_tried_by g p)) Gnat_config.provers

let apply_split_goal_if_needed g =
   (* before doing any proofs, we apply "split" to all "main goals" (see
      iter_main_goals). This function applies that transformation, but only
      when needed. *)
   if Session.PHstr.mem g.Session.goal_transformations first_transform
   then ()
   else
      ignore
        (Session.add_registered_transformation
           ~keygen:Keygen.keygen (get_session ()) first_transform g)

let do_scheduled_jobs callback =
   Gnat_sched.handle_proof_results callback

exception Found of Gnat_loc.loc

let extract_sloc main_goal =
   let task = Session.goal_task main_goal in
   let goal_ident = (Task.task_goal task).Decl.pr_name in
   let label_set = goal_ident.Ident.id_label in
   try
      Ident.Slab.iter (fun lab ->
        match Gnat_expl.read_label lab.Ident.lab_string with
        | Some Gnat_expl.Gp_Subp loc -> raise (Found (loc))
        | _ -> ()
      ) label_set;
      Gnat_util.abort_with_message ~internal:true
        (Pp.sprintf "could not find source location for subprogram %s"
        goal_ident.Ident.id_string)
   with Found l -> l

let init_subp_vcs subp =
   apply_split_goal_if_needed subp.subp_goal

let init () =
   let session_dir =
     let project_dir =
      try Session.get_project_dir Gnat_config.filename
      with Not_found ->
      Gnat_util.abort_with_message ~internal:true
        (Pp.sprintf "could not compute session file for %s" Gnat_config.filename)
     in
     match Gnat_config.proof_dir with
     | None -> project_dir
     | Some dir_name ->
        Filename.concat (Filename.concat dir_name "sessions")
                        (Filename.basename project_dir) in
   let env_session, is_new_session =
      (* either create a new session, or read an existing ession *)
      let session, is_new_session, use_shapes =
         if not Gnat_config.force && Sys.file_exists session_dir then
           let session, use_shapes = Session.read_session session_dir in
            session , false, use_shapes
         else
           Session.create_session session_dir, true, false in
      let ctxt = Session.mk_update_context
          ~allow_obsolete_goals:true
          ~release_tasks:false
          ~keep_unmatched_theories:(Gnat_config.limit_subp <> None)
          ~use_shapes_for_pairing_sub_goals:use_shapes
          Gnat_sched.Keygen.keygen
      in
      let env_session, (_:bool), (_:bool) =
         Session.update_session
           ~ctxt
           session
           Gnat_config.env
           Gnat_config.config in
      env_session, is_new_session in
   my_session := Some env_session;
   if is_new_session || not (has_file env_session) then begin
      let rel_filename =
        Sysutil.relativize_filename session_dir Gnat_config.filename
      in
      ignore
        (Session.add_file
          ~keygen:Gnat_sched.Keygen.keygen
          env_session rel_filename);
       end

let save_session () =
   Session.save_session Gnat_config.config (get_session ()).Session.session

let mk_subp_goal goal =
  { subp_goal = goal;
    subp_entity = extract_sloc goal
  }

let iter_subps f =
   let acc = ref [] in
   iter_main_goals (fun g ->
     if Session.goal_task_option g = None then ()
     else acc := mk_subp_goal g :: !acc);
   List.iter f !acc

let matches_subp_filter subp =
   match Gnat_config.limit_subp with
   | None -> true
   | Some lab ->
         let task = Session.goal_task subp.subp_goal in
         let goal_ident = (Task.task_goal task).Decl.pr_name in
         let label_set = goal_ident.Ident.id_label in
         Ident.Slab.mem lab label_set

module Save_VCs = struct

   exception Found of Whyconf.prover *  Call_provers.prover_result

   let find_successful_proof goal =
  (* given a goal, find a successful proof attempt for exactly this goal (not
     counting transformations *)
  try
    Session.PHprover.iter (fun prover pa ->
      match pa.Session.proof_obsolete, pa.Session.proof_state with
      | false, Session.Done
         ({ Call_provers.pr_answer = Call_provers.Valid } as pr) ->
          raise (Found (prover, pr))
      | _ -> ()) goal.Session.goal_external_proofs;
    raise Exit
  with Found (prover, pr) -> prover, pr


let add_to_prover_stat pr stat =
  (* add the result of the prover run to the statistics record for some prover
     *)
  stat.Gnat_report.count <- stat.Gnat_report.count + 1;
  if pr.Call_provers.pr_time > stat.Gnat_report.max_time then
    stat.Gnat_report.max_time <- pr.Call_provers.pr_time;
  if pr.Call_provers.pr_steps > stat.Gnat_report.max_steps then
    stat.Gnat_report.max_steps <- pr.Call_provers.pr_steps

let add_to_stat prover pr stat =
  (* add the result pr of the prover run of "prover" to the statistics table *)
  if Whyconf.Hprover.mem stat prover then
    add_to_prover_stat pr (Whyconf.Hprover.find stat prover)
  else
    Whyconf.Hprover.add stat prover
      { Gnat_report.count = 1;
        max_time = pr.Call_provers.pr_time;
        max_steps = pr.Call_provers.pr_steps }


   let rec extract_stat_goal stat goal =
     assert (goal.Session.goal_verified <> None);
     try
       let prover, pr = find_successful_proof goal in
       add_to_stat prover pr stat
     with Exit ->
       try
         Session.PHstr.iter (fun _ tr ->
           if tr.Session.transf_verified <> None then
             List.iter (extract_stat_goal stat) tr.Session.transf_goals;
          (* need to exit here so once we found a transformation that proves
           * the goal, don't try further *)
           raise Exit) goal.Session.goal_transformations;
       with Exit -> ()

   let extract_stats (obj : objective) =
     let stats = Whyconf.Hprover.create 5 in
     let obj_rec = Gnat_expl.HCheck.find explmap obj in
     GoalSet.iter (extract_stat_goal stats) obj_rec.toplevel;
     stats

   let count_map : (int ref) Gnat_expl.HCheck.t = Gnat_expl.HCheck.create 17

   module GM = GoalMap

   let goal_map : string GM.t = GM.create 17

   let find check =
      try Gnat_expl.HCheck.find count_map check
      with Not_found ->
         let r = ref 0 in
         Gnat_expl.HCheck.add count_map check r;
         r

   let vc_file goal =
      GM.find goal_map goal

   let with_fmt_channel filename f =
      let cout = open_out filename in
      let fmt  = Format.formatter_of_out_channel cout in
      f fmt;
      close_out cout

   let vc_name check prover =
      let r = find check in
      incr r;
      let n = !r in
      let count_str = if n = 1 then "" else string_of_int n in
      let ext = Driver.get_extension prover.Session.prover_driver in
      Pp.sprintf "%a%s%s" Gnat_expl.to_filename check count_str ext

   let save_vc ~cntexample goal prover =
      let check = get_objective goal in
      let task = Session.goal_task goal in
      let dr = prover.Session.prover_driver in
      let ce_prover =
        prover.Session.prover_config.Whyconf.prover.Whyconf.prover_name in
      let vc_fn = vc_name check prover in
      GM.add goal_map goal vc_fn;
      with_fmt_channel vc_fn
        (fun fmt ->
          Driver.print_task ~cntexample ~ce_prover dr fmt task)

   let compute_trace =
     let rec compute_trace acc f =
       let acc = Term.t_fold compute_trace acc f in
       match Gnat_expl.extract_sloc f.Term.t_label with
       (* it should be enough to look at the "sloc"s here, and not take into
          account the explanations. *)
       | Some loc -> Gnat_loc.S.add loc acc
       | _ -> acc
     in
     fun goal ->
       let f = Task.task_goal_fmla (Session.goal_task goal) in
       compute_trace Gnat_loc.S.empty f

   let save_trace goal =
      let check = get_objective goal in
      let trace_fn = Pp.sprintf "%a.trace" Gnat_expl.to_filename check in
      let trace = compute_trace goal in
      (* Do not generate an empty file if there is no location at all.
         Do not generate a file with a single location for the goal, as this
         is not useful. *)
      if Gnat_loc.S.cardinal trace > 1 then begin
        with_fmt_channel trace_fn (fun fmt ->
           Gnat_loc.S.iter (fun l ->
              Format.fprintf fmt "%a@." Gnat_loc.simple_print_loc
             (Gnat_loc.orig_loc l)) trace);
        (trace_fn, trace)
      end
      else ("", Gnat_loc.S.empty)

   (* Group of functions to build a json object for a session tree.
      More precisely a session forest, because we start with a list of
      goals for a given check. See gnat_report.mli for the JSON
      structure that we use here. *)
   let rec check_to_json obj =
     let obj_rec = Gnat_expl.HCheck.find explmap obj in
     let l = ref [] in
     GoalSet.iter (fun x -> l := goal_to_json x :: !l) obj_rec.toplevel;
     Json.List !l
   and goal_to_json g =
     let s = Mstr.empty in
     Json.Record
       (Mstr.add "proof_attempts" (proof_attempts_to_json g)
          (Mstr.add "transformations" (transformations_to_json g) s))
   and proof_attempts_to_json g =
     let s = Mstr.empty in
     let r = Session.PHprover.fold (fun prover pa acc ->
         let pr_name = prover.Whyconf.prover_name in
         match pa.Session.proof_obsolete, pa.Session.proof_state with
         | false, Session.Done pr ->
           Mstr.add pr_name (proof_result_to_json pr) acc
         | _, _ -> acc) g.Session.goal_external_proofs s in
     Json.Record r

   and proof_result_to_json r =
     let answer =
       Pp.sprintf "%a"
         Call_provers.print_prover_answer r.Call_provers.pr_answer in
     let s = Mstr.empty in
     let r =
       Mstr.add "time" (Json.Float r.Call_provers.pr_time)
         (Mstr.add "steps" (Json.Int r.Call_provers.pr_steps)
            (Mstr.add "result" (Json.String answer) s)) in
     Json.Record r
   and transformations_to_json g =
     let map =
       Session.PHstr.fold (fun tf_name tf acc ->
           Mstr.add tf_name (transformation_to_json tf) acc)
         g.Session.goal_transformations
         Mstr.empty
     in
     Json.Record map
   and transformation_to_json tf =
     Json.List (List.map goal_to_json tf.Session.transf_goals)

end

open Save_VCs

let goal_has_splits goal =
  not (Session.PHstr.is_empty goal.Session.goal_transformations)

let schedule_goal_with_prover ~cntexample g p =
(* actually schedule the goal, i.e., call the prover. This function returns
   immediately. *)
  if Gnat_config.debug then begin
    save_vc ~cntexample g p;
  end;
  Gnat_sched.run_goal ~cntexample p g

let schedule_goal ~cntexample g =
   (* actually schedule the goal, ie call the prover. This function returns
      immediately. *)
   let p = find_best_untried_prover g in
   schedule_goal_with_prover ~cntexample g p

let clean_automatic_proofs =
  let seen = GoalSet.empty () in
  (fun g ->
    if not (GoalSet.mem seen g) then begin
      GoalSet.add seen g;
      List.iter (fun prover ->
        let prover = prover.Session.prover_config.Whyconf.prover in
        Session.goal_iter (fun child ->
          match child with
          | Session.Proof_attempt pa ->
              if not pa.Session.proof_obsolete &&
                pa.Session.proof_prover = prover &&
                pa.Session.proof_limit = Gnat_config.limit
                     ~prover:prover.Whyconf.prover_name then
                  Session.remove_external_proof pa;
          | _ -> ()) g) Gnat_config.provers
    end)



let all_split_leaf_goals () =
  ()
  (* ??? disabled for now *)
(*
  iter_main_goals (fun g ->
    iter_leafs g
     (fun goal ->
      let is_registered =
         try ignore (get_objective goal); true
         with Not_found -> false in
      if is_registered then
         if is_full_split_goal goal then begin Save_VCs.save_vc goal end
         else begin
            let new_goals = further_split goal in
            if new_goals = [] then begin Save_VCs.save_vc goal end
            else begin
               List.iter (add_clone goal) new_goals;
               List.iter Save_VCs.save_vc new_goals
            end;
         end
      else ()
   ))
*)

let add_to_objective = add_to_objective ~toplevel:true
(* we mask the add_to_objective function here and fix it's toplevel argument to
   "true", so that outside calls always set toplevel to true *)

let session_proved_status obj =
   let obj_rec = Gnat_expl.HCheck.find explmap obj in
   try
     GoalSet.iter
       (fun x -> if x.Session.goal_verified = None then raise Exit)
       obj_rec.toplevel;
     true
   with Exit -> false

let finished_but_not_valid_or_unedited pa =
  (* return true if the proof attempt in argument has terminated, but did not
     prove the goal. *)
  match pa.Session.proof_state with
  | Session.Done { Call_provers.pr_answer = Call_provers.Valid } -> false
  | Session.Done _ -> true
  | Session.Unedited -> true
  | _ -> false

let is_valid_pa pa =
  match pa.Session.proof_state with
  | Session.Done { Call_provers.pr_answer = Call_provers.Valid } -> true
  | _ -> false

(* exception Goal_Found of goal *)
exception PA_Found of key Session.proof_attempt

let nothing _ =
  (* one-argument function which does nothing, is used below as dummy argument
     to traversal functions *)
  ()

let is_most_appropriate_prover obj_rec prover =
  if obj_rec.counter_example then begin
    match Gnat_config.prover_ce with
    | Some { Session.prover_config = { Whyconf.prover = p }} -> prover = p
    | _ -> true
  end else
    List.exists (fun p -> p.Session.prover_config.Whyconf.prover = prover)
    Gnat_config.provers

let select_appropriate_proof_attempt obj_rec pa =
(* helper function that helps finding the most appropriate proof attempt. In
  the normal case, we want to have an unsuccessful proof attempt of the
  counter example prover. If a CE prover is not available, we want a proof
  attempt that corresponds to a selected prover. *)
  if pa.Session.proof_obsolete then false
  else finished_but_not_valid_or_unedited pa &&
    is_most_appropriate_prover obj_rec pa.Session.proof_prover

let session_find_unproved_pa obj =
  let obj_rec = Gnat_expl.HCheck.find explmap obj in
  let rec aux goal =
    if goal.Session.goal_verified <> None then ()
    else begin
      Session.iter_goal nothing (Session.iter_transf aux) nothing goal;
      Session.iter_goal (fun pa ->
        if select_appropriate_proof_attempt obj_rec pa then
          raise (PA_Found pa)) nothing nothing goal
    end
  in
   try
     GoalSet.iter aux obj_rec.toplevel;
     None
   with PA_Found p ->
     Some p

let compute_replay_limit_from_pas pas =
  match pas with
  | { Call_provers.pr_steps = steps } ->
    let steps = steps + steps / 10 + 1 in
    { Call_provers.empty_limit with
      Call_provers.limit_steps = steps }

let for_some_proof_attempt pred map =
  try
    Session.PHprover.iter (fun _ pa -> if pred pa then raise Exit else ()) map;
    false
  with Exit -> true

let for_some_transformation pred map =
  try
    Session.PHstr.iter (fun _ tf -> if pred tf then raise Exit else ()) map;
    false
  with Exit -> true

let rec is_obsolete_verified goal =
  (* Check if a goal is or was verified, including using obsolete proofs *)
  goal.Session.goal_verified <> None ||
  for_some_proof_attempt is_valid_pa goal.Session.goal_external_proofs ||
    for_some_transformation
    (fun tf -> List.for_all is_obsolete_verified tf.Session.transf_goals)
    goal.Session.goal_transformations

let rec replay_transf tf =
  let tf_proves_goal =
    List.for_all (fun g -> g.Session.goal_verified <> None)
      tf.Session.transf_goals
  in
  if tf_proves_goal then List.iter replay_goal tf.Session.transf_goals
  else ()

and replay_goal goal =
  if not (is_obsolete_verified goal) then ()
  else
    try
      (* first try to find a proof_attempt that proves this goal entirely. This
       * will raise PA_Found if such a PA is found. *)
      Session.iter_goal (fun pa -> if is_valid_pa pa then raise (PA_Found pa))
        nothing nothing goal;
      (* we go here only if no such PA was found. We now replay the
         transformations *)
      Session.iter_goal nothing (replay_transf) nothing goal
    with PA_Found pa ->
      let prover =
        try
          Some (
          List.find (fun p ->
            p.Session.prover_config.Whyconf.prover = pa.Session.proof_prover)
            Gnat_config.provers)
        with Not_found ->
          Gnat_report.add_warning
          ("could not replay goal due to missing prover " ^
            pa.Session.proof_prover.Whyconf.prover_name);
          None in
      Opt.iter (fun prover ->
          let limit =
            match pa.Session.proof_state with
            | Session.Done pas -> compute_replay_limit_from_pas pas
            | _ -> assert false in
          Gnat_sched.run_goal ~cntexample:false ~limit prover goal) prover




let replay_obj obj =
  let obj_rec = Gnat_expl.HCheck.find explmap obj in
  GoalSet.iter replay_goal obj_rec.toplevel

let replay () =
  iter replay_obj
