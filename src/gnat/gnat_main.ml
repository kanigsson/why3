(* This is the main file of gnatwhy3 *)

(* Gnatwhy3 does the following:
   - it reads a .mlw file that was generated by gnat2why
   - it computes the VCs
   - it runs the selected provers on each VC.
   - it generates a summary of what was proved and not proved in JSON format
 and outputs this JSON format to stdout (for gnat2why to read).

   See gnat_objectives.mli for the notion of objective and goal.

   See gnat_report.mli for the JSON format

   gnat_main can be seen as the "driver" for gnatwhy3. Much of the
   functionality is elsewhere.
   Specifically, this file does:
      - compute the objective that belongs to a goal/VC
      - drive the scheduling of VCs, and handling of results
      - output the messages
*)

open Why3
open Term
open Gnat_scheduler

module C = Gnat_objectives.Make (Gnat_scheduler)

let rec is_trivial fml =
   (* Check wether the formula is trivial.  *)
   match fml.t_node with
   | Ttrue -> true
   | Tquant (_,tq) ->
         let _,_,t = t_open_quant tq in
         is_trivial t
   | Tlet (_,tb) ->
         let _, t = t_open_bound tb in
         is_trivial t
   | Tbinop (Timplies,_,t2) ->
         is_trivial t2
   | Tcase (_,tbl) ->
         List.for_all (fun b ->
            let _, t = t_open_branch b in
            is_trivial t) tbl
   | _ -> false

let register_goal s goal_id =
   (* Register the goal by extracting the explanation and trace. If the goal is
    * trivial, do not register *)
     let task = Session_itp.get_task s goal_id in
     let fml = Task.task_goal_fmla task in
     match is_trivial fml, Gnat_expl.search_labels fml with
     | true, None ->
         Gnat_objectives.set_not_interesting goal_id
     | false, None ->
         Gnat_util.abort_with_message ~internal:true
           "Task has no tracability label."
     | _, Some c ->
         if c.Gnat_expl.already_proved then
           Gnat_objectives.set_not_interesting goal_id
         else
           Gnat_objectives.add_to_objective c goal_id

let rec handle_vc_result c goal result =
   (* This function is called when the prover has returned from a VC.
       goal           is the VC that the prover has dealt with
       result         a boolean, true if the prover has proved the VC
       prover_result  the actual proof result, to extract statistics
   *)
   let obj, status = C.register_result c goal result in
   match status with
   | Gnat_objectives.Proved -> ()
   | Gnat_objectives.Not_Proved -> ()
   | Gnat_objectives.Work_Left ->
       List.iter (create_manual_or_schedule c obj) (Gnat_objectives.next obj)
   | Gnat_objectives.Counter_Example ->
     (* In this case, counterexample prover will be never None *)
     let prover_ce = (Opt.get Gnat_config.prover_ce) in
     C.schedule_goal_with_prover c ~cntexample:true ~callback:(interpret_result c) goal
       prover_ce

and interpret_result c pa pas =
   (* callback function for the scheduler, here we filter if an interesting
      goal has been dealt with, and only then pass on to handle_vc_result *)
   match pas with
   | Controller_itp.Done r ->
     let session = c.Controller_itp.controller_session in
     let goal = Session_itp.get_proof_attempt_parent session pa in
     let answer = r.Call_provers.pr_answer in
     if answer = Call_provers.HighFailure && Gnat_config.debug &&
        not (Gnat_config.is_ce_prover session pa) then
       Gnat_report.add_warning r.Call_provers.pr_output;
     handle_vc_result c goal (answer = Call_provers.Valid)
   | _ ->
         ()

and create_manual_or_schedule (c: Controller_itp.controller) _obj goal =
  let s = c.Controller_itp.controller_session in
  match Gnat_config.manual_prover with
  | Some _ when C.goal_has_splits s goal &&
                not (Session_itp.pn_proved c.Controller_itp.controller_session goal) ->
                  handle_vc_result c goal false
(* TODO recover this
    | Some p when Gnat_manual.is_new_manual_proof goal &&
                not (C.pn_proved c goal) ->
                  let _ = Gnat_manual.create_prover_file goal obj p in
                  ()*)
  | _ -> schedule_goal c goal

and schedule_goal (c: Controller_itp.controller) (g : Session_itp.proofNodeID) =
   (* schedule a goal for proof - the goal may not be scheduled actually,
      because we detect that it is not necessary. This may have several
      reasons:
         * command line given to skip proofs
         * goal already proved
         * goal already attempted with identical options
   *)
   if (Gnat_config.manual_prover <> None
       && not (Session_itp.pn_proved c.Controller_itp.controller_session g)) then begin
       actually_schedule_goal c g
   (* then implement reproving logic *)
   end else begin
     (* Maybe the goal is already proved *)
      if Session_itp.pn_proved c.Controller_itp.controller_session g then begin
         handle_vc_result c g true
      (* Maybe there was a previous proof attempt with identical parameters *)
      end else if Gnat_objectives.all_provers_tried c.Controller_itp.controller_session g then begin
         (* the proof attempt was necessarily false *)
         handle_vc_result c g false
      end else begin
         actually_schedule_goal c g
      end;
   end

and actually_schedule_goal c g =
  C.schedule_goal ~cntexample:false ~callback:(interpret_result c) c g

let handle_obj c obj =
   if Gnat_objectives.objective_status obj <> Gnat_objectives.Proved then begin
     match Gnat_objectives.next obj with
      | [] -> ()
      | l ->
         List.iter (create_manual_or_schedule c obj) l
   end

let all_split_subp c subp =
  let s = c.Controller_itp.controller_session in
  if C.matches_subp_filter s subp then begin
     C.init_subp_vcs c subp;
     Gnat_objectives.iter_leaf_goals s subp (register_goal s);
     C.all_split_leaf_goals ();
     Gnat_objectives.clear ()
   end

let filter_model m trace =
  if trace = Gnat_loc.S.empty then
    m
  else
    let trace_to_list trace =
    (* Build list of locations (pairs of filename and line number) from trace *)
      Gnat_loc.S.fold
        (fun loc list ->
          let sloc = Gnat_loc.orig_loc loc in
          let col = Gnat_loc.get_col sloc in
          let pos = Why3.Loc.user_position
            (Gnat_loc.get_file sloc) (Gnat_loc.get_line sloc) col col in
          (pos::list)
        )
        trace
        [] in
    let positions = trace_to_list trace in
    Model_parser.model_for_positions_and_decls m ~positions

let report_messages c obj =
  let s = c.Controller_itp.controller_session in
  let result =
    if C.session_proved_status c obj  then
      Gnat_report.Proved (C.Save_VCs.extract_stats c obj)
    else
      let unproved_pa = C.session_find_unproved_pa c obj in
      let unproved_goal =
        Opt.map (fun pa -> Session_itp.get_proof_attempt_parent s pa) unproved_pa in
      let unproved_task = Opt.map (Session_itp.get_task s) unproved_goal in
      let (tracefile, trace) =
        match unproved_goal, Gnat_config.proof_mode with
        | Some goal, (Gnat_config.Progressive | Gnat_config.Per_Path) ->
            C.Save_VCs.save_trace s goal
        | _ -> ("", Gnat_loc.S.empty) in
      let model =
        let unproved_pa = Opt.map (Session_itp.get_proof_attempt_node s) unproved_pa in
        match unproved_pa with
        | Some { Session_itp.proof_state =
                   Some {Call_provers.pr_answer =
                         Call_provers.Unknown (_, Some Call_provers.Resourceout)}} ->
            (* Resource limit was hit, the model is not useful *)
            None
        | Some { Session_itp.proof_state =
                  Some ({Call_provers.pr_answer = _} as r)} ->
          Some (filter_model r.Call_provers.pr_model trace)
        | _ -> None
      in
      (* TODO recover manual
      let manual_info =
        match unproved_pa with
        | None -> None
        | Some pa -> Gnat_manual.manual_proof_info pa in
       *)
      Gnat_report.Not_Proved (unproved_task, model, tracefile, None (* TODO manual_info *)) in
  Gnat_report.register obj (C.Save_VCs.check_to_json s obj) result

let c = Gnat_objectives.init_cont ()

let _ =
   (* This is the main code. We read the file into the session if not already
      done, we apply the split_goal transformation when needed, and we schedule
      the first VC of all objectives. When done, we save the session.
   *)

   (* save session on interrupt initiated by the user *)
   let save_session_and_exit c signum =
     (* ignore all SIGINT, SIGHUP and SIGTERM, which may be received when
        gnatprove is called in GPS, so that the session file is always saved *)
     Sys.set_signal Sys.sigint Sys.Signal_ignore;
     Sys.set_signal Sys.sighup Sys.Signal_ignore;
     Sys.set_signal Sys.sigterm Sys.Signal_ignore;
     C.save_session c;
     exit signum
   in

   (* TODO have to do it after initialization of controller *)
   Sys.set_signal Sys.sigint (Sys.Signal_handle (save_session_and_exit c));
   Util.init_timing ();
   Util.timing_step_completed "gnatwhy3.init"

let normal_handle_one_subp c subp =
   let s = c.Controller_itp.controller_session in
   if C.matches_subp_filter s subp then begin
     C.init_subp_vcs c subp;
     Gnat_scheduler.wait_for_idle ();
     let s = c.Controller_itp.controller_session in
     (* TODO wait until finished *)
     Gnat_objectives.iter_leaf_goals s subp (register_goal s)
   end

let _ =
   begin
   try
     match Gnat_config.proof_mode with
     | Gnat_config.Progressive
     | Gnat_config.Per_Path
     | Gnat_config.Per_Check ->
        C.iter_subps c (normal_handle_one_subp c);
        Util.timing_step_completed "gnatwhy3.register_vcs";
        if Gnat_config.replay then begin
          ()
          (* TODO replay Reimplement
          Gnat_objectives.replay ();
          Gnat_objectives.do_scheduled_jobs (fun _ _ -> ()); *)
        end else begin
          Gnat_objectives.iter (handle_obj c);
          Util.timing_step_completed "gnatwhy3.schedule_vcs";
        end;
     | Gnat_config.All_Split ->
        C.iter_subps c (all_split_subp c)
     | Gnat_config.No_WP ->
        (* we should never get here *)
        ()
    with e when Debug.test_flag Debug.stack_trace -> raise e
    | e ->
       let s = Pp.sprintf "%a.@." Exn_printer.exn_printer e in
       Gnat_util.abort_with_message ~internal:true s
   end

(* This is to be executed when scheduling ends *)
let ending () =
  Util.timing_step_completed "gnatwhy3.run_vcs";
  C.save_session c;
  Util.timing_step_completed "gnatwhy3.save_session";
  Gnat_objectives.iter (report_messages c);
  Gnat_report.print_messages ();
   (* Dump profiling data (when compiled with profiling enabled) to file whose
      name is based on the processed .mlw file; otherwise profile data from
      several compilation would be written to a single gmon.out file and
      overwrite each other. When compiled with profiling disabled it has no
      visible effect. Note: we set the filename just before the program exit
      to not interfere with profiling of provers.
   *)
   let basename =
     Filename.chop_extension
       (Filename.basename Gnat_config.filename) in
   Unix.putenv "GMON_OUT_PREFIX" (basename ^ "_gnatwhy3_gmon.out")


let _ =
   (*Gnat_scheduler.timeout ~ms:125 beginning;*)
   (*Gnat_scheduler.timeout ~ms:125 ending;*)
   Gnat_scheduler.main_loop ending
