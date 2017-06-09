open Why3
open Format
open Itp_communication
open Json_util

module Protocol_SPARKITP = struct

  let requests = ref []

  let push_request_string (s: string) =
    let r = parse_request s in
    requests := r :: !requests

  let push_request r =
    requests := r :: !requests

  let get_requests () =
    let l = !requests in
    requests := [];
    List.rev l

(* TODO think of a better way of separating stuff than > *)
  let notify n =
    Format.printf "%a>>>>@." print_notification n

end

module TODO_Scheduler = struct

(* the private list of functions to call on idle, sorted higher
       priority first. *)
let idle_handler : (int * (unit -> bool)) list ref = ref []

(* [insert_idle_handler p f] inserts [f] as a new function to call
       on idle, with priority [p] *)
let insert_idle_handler p f =
  let rec aux l =
    match l with
    | [] -> [p,f]
    | (p1,_) as hd :: rem ->
       if p > p1 then (p,f) :: l else hd :: aux rem
  in
  idle_handler := aux !idle_handler

(* the private list of functions to call on timeout, sorted on
       earliest trigger time first. *)
let timeout_handler : (float * float * (unit -> bool)) list ref = ref []

(* [insert_timeout_handler ms t f] inserts [f] as a new function to call
       on timeout, with time step of [ms] and first call time as [t] *)
let insert_timeout_handler ms t f =
  let rec aux l =
    match l with
    | [] -> [ms,t,f]
    | (_,t1,_) as hd :: rem ->
       if t < t1 then (ms,t,f) :: l else hd :: aux rem
  in
  timeout_handler := aux !timeout_handler

(* public function to register a task to run on idle *)
let idle ~(prio:int) f = insert_idle_handler prio f

(* public function to register a task to run on timeout *)
let timeout ~ms f =
  assert (ms > 0);
  let ms = float ms /. 1000.0 in
  let time = Unix.gettimeofday () in
  insert_timeout_handler ms (time +. ms) f

(* buffer for storing character read on stdin *)
let buf = Bytes.create 1024

let main_loop treat_requests =
  (* attempt to run the first timeout handler *)
  let time = Unix.gettimeofday () in
  while true do
    match !timeout_handler with
     | (ms,t,f) :: rem when t <= time ->
        timeout_handler := rem;
        let b = f () in
        let time = Unix.gettimeofday () in
        if b then insert_timeout_handler ms (ms +. time) f
     | _ ->
           (* no idle handler *)
(*
           eprintf "check connection for a some delay@.";
*)
           let delay =
             match !timeout_handler with
             | [] -> 0.125
             (* 1/8 second by default *)
             | (_,t,_) :: _ -> t -. time
             (* or the time left until the next timeout otherwise *)
           in
           begin
             let (todo, _, _) = Unix.select [Unix.stdin] [] [] delay in
             (* TODO maximum size of request for now *)
             if todo == [Unix.stdin] then
               let n = Unix.read Unix.stdin buf 0 1024 in
               if n = 0 then
                 begin
                   (* attempt to run the first idle handler *)
                   match !idle_handler with
                   | (p,f) :: rem ->
                       idle_handler := rem;
                       let b = f () in
                       if b then insert_idle_handler p f
                   | [] -> ()
                 end
               else
                 let s = Bytes.sub_string buf 0 (n-1) in
                 (* TODO here maybe empty completely stdin ? *)
                 treat_requests s
             else
               begin
                 (* attempt to run the first idle handler *)
                 match !idle_handler with
                 | (p,f) :: rem ->
                     idle_handler := rem;
                     let b = f () in
                     if b then insert_idle_handler p f
                 | [] -> ()
               end
           end
  done
end

module Server = Itp_server.Make (TODO_Scheduler) (Protocol_SPARKITP)

(************************)
(* parsing command line *)
(************************)

let files : string Queue.t = Queue.create ()
let opt_parser = ref None

let spec = Arg.align [
  "-F", Arg.String (fun s -> opt_parser := Some s),
      "<format> select input format (default: \"why\")";
  "--format", Arg.String (fun s -> opt_parser := Some s),
      " same as -F";
(*
  "-f",
   Arg.String (fun s -> input_files := s :: !input_files),
   "<file> add file to the project (ignored if it is already there)";
*)
  Termcode.arg_extra_expl_prefix
]

let usage_str = sprintf
  "Usage: %s [options] [<file.why>|<project directory>]..."
  (Filename.basename Sys.argv.(0))

let env, gconfig =
  Gnat_config.env, Gnat_config.config


(* Initialization of config, provers, task_driver and controller in the server *)
let () =
  Queue.add Gnat_config.filename files;
  let dir =
    try
      Server_utils.get_session_dir ~allow_mkdir:true files
    with Invalid_argument s ->
      Format.eprintf "Error: %s@." s;
      Whyconf.Args.exit_with_usage spec usage_str
  in
  Server.init_server gconfig env dir

(* TODO *)
(***********************)
(* start the daemon    *)
(***********************)


let () =
  TODO_Scheduler.main_loop Protocol_SPARKITP.push_request_string
