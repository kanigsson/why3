open Why3

type msg =
  { check      : Gnat_expl.check;
    result     : bool;
    time       : float;
    steps      : int;
    extra_info : int option;
    tracefile  : string;
    vc_file    : string option;
  }

let msg_set : msg list ref = ref []

let is_digit c =
  match c with
  | '0' .. '9' -> true
  | _ -> false

let extract_steps s =
  (* extract steps from alt-ergo "valid" output; return None if output is not
     recognized, or no steps information present *)
  (* We simply search for (xxx) at the end of the first line of the  output,
     where all the xxx must be digits. *)
  let s =
    try Strings.slice s 0 (String.index s '\n' )
    with Not_found -> s
  in
  try
    let len = String.length s in
    if len = 0 then None
    else
      let i = ref (len - 1) in
      (* skip spaces *)
      while s.[!i] = ' ' do
        i := !i - 1;
      done;
      if s.[!i] = ')' then begin
        let max = !i in
        while !i > 0 && is_digit s.[!i-1] do
          i := !i - 1;
        done;
        if !i > 0 && s.[!i-1] = '(' then
          let s = Strings.slice s !i max in
          Some (int_of_string s)
        else None
      end else None
  with _ -> None

let extract_steps_fail s =
  if Strings.starts_with s "steps:" then
    try Some (int_of_string (Strings.slice s 6 (String.length s)))
    with _ -> None
  else None

let register check task result valid filename tracefile =
  let time =
    match result with
    | None -> 0.0
    | Some r -> r.Call_provers.pr_time in
  let steps =
    match result with
    | Some ({Call_provers.pr_answer = Call_provers.Valid } as r) ->
        begin match extract_steps r.Call_provers.pr_output with
        | Some steps -> steps
        | None -> 0
        end
    | Some { Call_provers.pr_answer = Call_provers.Failure s } ->
        begin match extract_steps_fail s with
        | Some steps -> steps
        | None -> 0
        end
    | _ -> 0 in
  let extra_info =
    if valid then None
    else begin match task with
      | None -> None
      | Some t -> Gnat_expl.get_extra_info t
    end in
  let msg =
  { check         = check;
    result        = valid;
    extra_info    = extra_info;
    time          = time;
    steps         = steps;
    tracefile     = tracefile;
    vc_file       = filename } in
  msg_set := msg :: !msg_set

let get_info info  =
    match info with
    | None -> 0
    | Some info -> info

(* The function replaces %{f,t,T,m,l,d} to their corresponding values
   in the string cmd.
   This function is based on the Call_provers.actualcommand, for
   some reason not in the Why3 API nor really convenient *)
let actual_editor_cmd ?main filename cmd =
  let m = match main with
    | None -> Whyconf.get_main Gnat_config.config
    | Some m -> m in
  let replace_func s =
    match (Str.matched_string s).[1] with
    | '%' -> "%"
    | 'f' -> Sys.getcwd () ^ Filename.dir_sep ^ filename
    (* Can %t and %T be on an editor command line and have a meaning?
       Is it allowed by Why3config? *)
    | 't' -> string_of_int (Whyconf.timelimit m)
    | 'T' -> string_of_int (succ (Whyconf.timelimit m))
    | 'm' -> string_of_int (Whyconf.memlimit m)
    | 'l' -> Whyconf.libdir m
    | 'd' -> Whyconf.datadir m
    | a ->  Char.escaped a in
  Str.global_substitute (Str.regexp "%.") replace_func cmd

let string fmt s = Format.fprintf fmt "\"%s\"" s
let int fmt d = Format.fprintf fmt "%d" d
let bool fmt b = Format.fprintf fmt "%b" b

let print_json_field key value_pr fmt value =
  Format.fprintf fmt "%a : %a " string key value_pr value

let print_trace_file fmt trace  =
  if trace = "" then ()
  else begin
    Format.fprintf fmt ", ";
    print_json_field "tracefile" string fmt trace
  end

let print_vc_file_info fmt vc_file =
  match vc_file with
  | None -> ()
  | Some name ->
      print_json_field "vc_file" string fmt
                    (Sys.getcwd () ^ Filename.dir_sep ^ name);
      Format.fprintf fmt ",";
     let editor = Gnat_config.prover_editor () in
     let cmd_line =
       List.fold_left (fun str s -> str ^ " " ^ s) editor.Whyconf.editor_command
                      editor.Whyconf.editor_options in
     print_json_field "editor_cmd" string fmt (actual_editor_cmd name cmd_line);
     Format.fprintf fmt ","

let print_json_msg fmt m =
  Format.fprintf fmt "{%a, %a, %a, %a%a%a}"
    (print_json_field "id" int) m.check.Gnat_expl.id
    (print_json_field "reason" string)
      (Gnat_expl.reason_to_ada m.check.Gnat_expl.reason)
    (print_json_field "result" bool) m.result
    (print_json_field "extra_info" int) (get_info m.extra_info)
    print_trace_file m.tracefile
    print_vc_file_info m.vc_file

let print_msg_list fmt l =
  Pp.print_list_delim ~start:Pp.lsquare ~stop:Pp.rsquare ~sep:Pp.comma
  print_json_msg fmt l

let print_messages () =
  Format.printf "%a@." print_msg_list !msg_set
