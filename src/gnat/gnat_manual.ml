open Why3

type goal = Session_itp.proofNodeID
type attempt = Session_itp.proofAttemptID

(* TODO unused
let filename_limit = 246
*)

let manual_attempt_of_goal s goal =
  match Gnat_config.manual_prover with
  | None -> None
  | Some p ->
      let proof_attempts = Session_itp.get_proof_attempt_ids s goal in
      Whyconf.Hprover.find_opt proof_attempts p

let is_new_manual_proof session goal =
  manual_attempt_of_goal session goal = None

(* Returns the theory encapsulating the goal *)
(* TODO unused
let rec find_goal_theory s goal =
  let parent = Session_itp.get_proof_parent s goal in
  match parent with
  | Session_itp.Theory th -> th
  | Session_itp.Trans tr ->
    let tr_parent = Session_itp.get_trans_parent s tr in
    find_goal_theory s tr_parent
*)

(* TODO unused done in Why3 !?
let get_file_extension filename =
  try
    let name = Filename.chop_extension filename in
    let ext = String.sub filename (String.length name)
                         (String.length filename - String.length name) in
    ext
  with
  | Invalid_argument _ -> ""
*)

(* TODO unused
let prover_files_dir proj prover =
  let wc_prover = prover.Session.prover_config.Whyconf.prover in
  match Gnat_config.proof_dir with
  | None -> ""
  | Some dir ->
     let prover_dir = (Filename.concat
                         dir
                         wc_prover.Whyconf.prover_name)
     in
     if not (Sys.file_exists prover_dir) then
       Unix.mkdir prover_dir 0o750;
     let punit_dir = Filename.concat prover_dir proj in
     if not (Sys.file_exists punit_dir) then
       Unix.mkdir punit_dir 0o750;
     Sysutil.relativize_filename (Sys.getcwd ()) punit_dir
*)

(* TODO unused
let resize_shape sh limit =
  let index = ref 0 in
  let sh_len = String.length sh in
  let separator_re = Str.regexp "__" in
  (try
    while (sh_len - !index) >= limit do
      index := (Str.search_forward separator_re sh !index) + 2
    done;
    String.sub sh !index (sh_len - !index)
  with
  | _ -> "")
*)

(* TODO unused
let compute_filename s contain_dir theory goal expl prover driver =
  let th_name_no_sanit = (Session_itp.theory_name theory).Ident.id_string in
  let why_fn =
    Driver.file_of_task driver
                        th_name_no_sanit
                        (Session_itp.theory_parent s theory).Session_itp.file_name
                        (Session_itp.get_task s goal) in
  let ext = get_file_extension why_fn in
  let thname = (Ident.sanitizer Ident.char_to_alnumus
                                Ident.char_to_alnumus
                                th_name_no_sanit) in
  (* Remove __subprogram_def from theory name *)
  let thname = String.sub thname 0 ((String.length thname) - 16) in

  (* Prevent generated filename from exceeding usual filesystems limit.
     2 character are reserved to differentiate files having name
     collision *)
  let shape = resize_shape expl.Gnat_expl.shape
                           (filename_limit - ((String.length thname)
                                              + (String.length ext) + 2)) in
  let noext = Filename.concat contain_dir
                              (Pp.sprintf "%s__%s" thname shape)
  in
  let num = ref 0 in
  let filename = ref (noext ^ ext) in
  while Sys.file_exists !filename do
    num := !num + 1;
    filename := noext ^ string_of_int !num ^ ext;
  done;
  !filename
*)

(* TODO to be redone when we have something for manual_provers
let create_prover_file s goal expl prover driver =
  let th = find_goal_theory s goal in
  let proj_name = Filename.basename (Session_itp.get_dir s) in
  let filename =
    compute_filename s (prover_files_dir proj_name prover) th goal expl prover driver in
  let cout = open_out filename in
  let fmt = Format.formatter_of_out_channel cout in
  let task = Session_itp.get_task s goal in
  Driver.print_task prover.Session.prover_driver fmt task;
  close_out cout;
  let pa = add_external_proof ~keygen:Gnat_sched.Keygen.keygen ~obsolete:false
                             ~archived:false ~limit:Call_provers.empty_limit
                             ~edit:(Some filename) goal
                             prover.Session.prover_config.Whyconf.prover
                             Unedited in
  pa
*)

(* ??? maybe use a Buffer.t? Isn't there some code already doing this in Why3?
 * *)
(* TODO unused
let editor_command (prover: Whyconf.prover) fn =
  (* this function loads the editor for a given prover, otherwise returns a
     default value *)
  let editor =
    let config_prover = Whyconf.get_prover_config Gnat_config.config prover in
    try Whyconf.editor_by_id Gnat_config.config
        config_prover.Whyconf.editor
    with Not_found ->
      { Whyconf.editor_name = "";
        editor_command = "";
        editor_options = [] }
  in
  let cmd_line =
    List.fold_left (fun str s -> str ^ " " ^ s)
                  editor.Whyconf.editor_command
                  editor.Whyconf.editor_options in
  Gnat_config.actual_cmd fn cmd_line
*)

(* TODO redo this when we have manual_provers
let manual_proof_info pa =
  match pa.proof_edited_as with
  | None -> None
  | Some fn ->
      let base_prover = pa.Session.proof_prover in
      let real_prover =
        List.find (fun p ->
          p = base_prover)
        Gnat_config.provers in
      Some (fn, editor_command real_prover fn)
*)
