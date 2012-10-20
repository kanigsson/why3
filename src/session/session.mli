(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2012   --   INRIA - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(** Proof sessions *)
(** Define all the functions needed for managing a session:
    Creation, saving, loading, modification, ...
    All the operations are immediately done.
    Use session_scheduler if you want to queue the operations
*)

open Stdlib

val debug : Debug.flag
(** The debug flag "session" *)

module PHstr : Util.PrivateHashtbl with type key = string
module PHprover : Util.PrivateHashtbl with type key = Whyconf.prover

(** {2 Proof attempts} *)

(** State of a proof *)
type proof_attempt_status =
    | Unedited (** editor not yet run for interactive proof *)
    | JustEdited (** edited but not run yet *)
    | Interrupted (** external proof has never completed *)
    | Scheduled (** external proof attempt is scheduled *)
    | Running (** external proof attempt is in progress *)
    | Done of Call_provers.prover_result (** external proof done *)
    | InternalFailure of exn (** external proof aborted by internal error *)

type expl
(** An explanation gives hint about how the goal has been produced.
    Allow to reattach proof_attempt to goal when the source file has been
    modified.
*)

type task_option
(** Currently just an option on a task, but later perhaps
    we should be able to release a task and rebuild it when needed *)


type ident_path =
  { ip_library : string list;
    ip_theory : string;
    ip_qualid : string list;
  }

type meta_args = Theory.meta_arg list
module Mmeta_args : Map.S with type key = meta_args
module Smeta_args : Mmeta_args.Set
type metas_args =  Smeta_args.t Util.Mstr.t
module Mmetas_args : Map.S with type key = metas_args

type idpos = {
  idpos_ts : ident_path Ty.Mts.t;
  idpos_ls : ident_path Term.Mls.t;
  idpos_pr : ident_path Decl.Mpr.t;
}

(** {2 Session} *)

(** All the element of a session contain a key which can hold whatever
    information the user want. It is generated by the keygen argument
    of the functions of this module *)

type 'a goal = private
    { mutable goal_key  : 'a;
      goal_name : Ident.ident; (** The ident of the task *)
      goal_expl : expl;
      goal_parent : 'a goal_parent;
      mutable goal_checksum : Termcode.checksum;  (** checksum of the task *)
      mutable goal_shape : Termcode.shape;  (** shape of the task *)
      mutable goal_verified : bool;
      goal_task: task_option;
      mutable goal_expanded : bool;
      goal_external_proofs : 'a proof_attempt PHprover.t;
      goal_transformations : 'a transf PHstr.t;
      mutable goal_metas : 'a metas Mmetas_args.t;
    }

and 'a proof_attempt = private
    { proof_key : 'a;
      mutable proof_prover : Whyconf.prover;
      proof_parent : 'a goal;
      mutable proof_state : proof_attempt_status;
      mutable proof_timelimit : int;
      mutable proof_memlimit : int;
      mutable proof_obsolete : bool;
      mutable proof_archived : bool;
      mutable proof_edited_as : string option;
    }

and 'a goal_parent = private
                     | Parent_theory of 'a theory
                     | Parent_transf of 'a transf
                     | Parent_metas  of 'a metas

and 'a metas =
  { mutable metas_key : 'a;
    metas_added : metas_args;
    metas_idpos : idpos;
    metas_parent : 'a goal;
    mutable metas_verified : bool;
    mutable metas_goal : 'a goal;
    (** Not mutated after the creation *)
    mutable metas_expanded : bool;
  }

and 'a transf = private
    { mutable transf_key : 'a;
      transf_name : string;
      (** Why3 tranformation name *)
      transf_parent : 'a goal;
      mutable transf_verified : bool;
      mutable transf_goals : 'a goal list;
      (** Not mutated after the creation *)
      mutable transf_expanded : bool;
    }

and 'a theory = private
    { mutable theory_key : 'a;
      theory_name : Ident.ident;
      theory_parent : 'a file;
      mutable theory_goals : 'a goal list;
      (** Not mutated after the creation *)
      mutable theory_verified : bool;
      mutable theory_expanded : bool;
    }

and 'a file = private
    { mutable file_key : 'a;
      file_name : string;
      file_format : string option;
      file_parent : 'a session;
      mutable file_theories: 'a theory list;
      (** Not mutated after the creation *)
      mutable file_verified : bool;
      mutable file_expanded : bool;
    }

and 'a session = private
    { session_files : 'a file PHstr.t;
      mutable session_shape_version : int;
      session_dir   : string;
    }

val print_session : Format.formatter -> 'a session -> unit
(** Print a session with a pstree format (cf Tree module) *)

val print_attempt_status : Format.formatter -> proof_attempt_status -> unit

val print_external_proof : Format.formatter -> 'key proof_attempt -> unit

val create_session : ?shape_version:int -> string -> 'key session
(** create a new_session in the given directory. The directory is
    created if it doesn't exists yet. Don't change the current
    directory of the program if you give a relative path *)

val get_project_dir : string -> string
(** find the session which correspond to the given file or return
    directly the given directory;
    return {Not_found} if the file or the directory doesn't exists
*)

(** {2 Read/Write} *)

type notask
(** A phantom type which is used for session which doesn't contain task. The
    only session that can not contain task are session that come from the
    following function *)

val read_session : string -> notask session
(** Read a session stored on the disk. It returns a session without any
    task attached to goals *)

val save_session : Whyconf.config -> 'key session -> unit
(** Save a session on disk *)

(** {2 Context of a session} *)

(** A session which contains task and proof_attempt depends on an
    environment and a prover configuration.
    Loaded provers are cached in order to load drivers once *)

type loaded_prover =
    { prover_config : Whyconf.config_prover;
      prover_driver : Driver.driver}

type loaded_provers = loaded_prover option PHprover.t

type 'a env_session = private
    { env : Env.env;
      mutable whyconf : Whyconf.config;
      loaded_provers : loaded_provers;
      session : 'a session}

val update_env_session_config : 'a env_session -> Whyconf.config -> unit
(** updates the configuration *)

val load_prover : 'a env_session -> Whyconf.prover -> loaded_prover option
(** load a prover *)

val unload_provers : 'a env_session -> unit
(** forces unloading of all provers,
    to force reading again the configuration *)

(** {2 Update session} *)

type 'key keygen = ?parent:'key -> unit -> 'key
(** type of functions which can generate keys *)

exception OutdatedSession

val update_session : keygen:'a keygen ->
  allow_obsolete:bool -> 'b session ->
  Env.env -> Whyconf.config -> 'a env_session * bool
(** reload the given session with the given environnement :
    - the files are reloaded
    - apply again the transformation
    - if some goals appear try to find to which goal
    in the given session it corresponds.

    The last case meant that the session was obsolete.
    It is authorized if [allow_obsolete] is [true],
    otherwise the exception [OutdatedSession] is raised.
    If the session was obsolete is indicated by
    the second result.

    raises [Failure msg] if the database file cannot be read correctly

*)

(** {2 Copy/Paste } *)

val copy_proof:  'a proof_attempt -> 'a proof_attempt
val copy_transf: 'a transf        -> 'a transf
val copy_metas:  'a metas         -> 'a metas
(** keys are copied *)

val add_proof_to_goal :
  keygen:'a keygen -> 'a env_session ->
  'a goal -> 'a proof_attempt ->'a proof_attempt
val add_transf_to_goal:
  keygen:'a keygen -> 'a env_session ->
  'a goal -> 'a transf        -> 'a transf
val add_metas_to_goal :
  keygen:'a keygen -> 'a env_session ->
  'a goal -> 'a metas         -> 'a metas
(** keys are normally generated *)

(** {2 Accessor} *)

exception NoTask
val goal_task : 'key goal -> Task.task
(** Return the task of a goal. Raise NoTask if the goal doesn't contain a task
    (equivalent to 'key = notask) *)

val goal_task_option : 'key goal -> Task.task option
(** Return the task of a goal. *)

val goal_expl : 'key goal -> string
(** Return the explication of a goal *)

val proof_verified : 'key proof_attempt -> bool
(** Return true if the proof is not obsolete and the result is valid *)


val get_used_provers : 'a session -> Whyconf.Sprover.t
(** Get the set of provers which appear in the session *)

(* val metas_of_virtuals : 'a metas -> Theory.Smeta.t *)
(* (\** Get the set of metas added (the parent goal must contain a task) *\) *)

(** {2 Modificator} *)

val set_transf_expanded : 'key transf -> bool -> unit
val set_metas_expanded : 'key metas -> bool -> unit
val set_goal_expanded : 'key goal -> bool -> unit
val set_theory_expanded : 'key theory -> bool -> unit
val set_file_expanded : 'key file -> bool -> unit
(** open one level or close all the sub-level *)

(** {2 General type} *)

type 'a any =
  | File of 'a file
  | Theory of 'a theory
  | Goal of 'a goal
  | Proof_attempt of 'a proof_attempt
  | Transf of 'a transf
  | Metas of 'a metas

val print_any : Format.formatter -> 'a any -> unit
(** Print a subtree with a pstree format (cf Tree module) *)

val key_any : 'a any -> 'a
(** return the key of an element of the tree *)

(** {2 External proof} *)

type 'key notify = 'key any -> unit
(** type of functions which notify modification of the verified field *)

val add_external_proof :
  ?notify:'key notify ->
  keygen:'key keygen ->
  obsolete:bool ->
  archived:bool ->
  timelimit:int ->
  memlimit:int ->
  edit:string option ->
  'key goal ->
  Whyconf.prover ->
  proof_attempt_status ->
  'key proof_attempt

val remove_external_proof : ?notify:'key notify -> 'key proof_attempt -> unit

val set_proof_state :
  ?notify:'key notify ->
  obsolete:bool ->
  archived:bool ->
  proof_attempt_status ->
  'key proof_attempt -> unit

val change_prover : 'a proof_attempt -> Whyconf.prover -> unit

val set_obsolete : ?notify:'key notify -> 'key proof_attempt -> unit

val set_archived : 'key proof_attempt -> bool -> unit

val set_edited_as : string option -> 'key proof_attempt -> unit

val get_edited_as_abs : 'key session -> 'k proof_attempt -> string option
(** return the edited filename after concatenation to session_dir *)

val update_edit_external_proof :
  'key env_session -> 'key proof_attempt -> string
(** return the absolute path of the edited file update with the
    current goal *)


val set_timelimit : int -> 'key proof_attempt -> unit
val set_memlimit : int -> 'key proof_attempt -> unit

val copy_external_proof :
  ?notify:'key notify ->
  keygen:'key keygen ->
  ?obsolete:bool ->
  ?archived:bool ->
  ?timelimit:int ->
  ?memlimit:int ->
  ?edit:string option ->
  ?goal:'key goal ->
  ?prover:Whyconf.prover ->
  ?attempt_status:proof_attempt_status ->
  ?env_session:'key env_session ->
  ?session:'key session ->
  'key proof_attempt -> 'key proof_attempt
(** copy an external proof.
    if env_session and session are given only env_session.session is
    taken into account.
    The edited file is copied and an env_session is not required if :
    {ul
    {- the goal is not modified}
    {- the prover is not modified}
    {- a session is given}
    }
    The edited file is regenerated if
    {ul
    {- the external proof contain an edited file}
    {- an env_session is given}
    {- the given goal (or the old one if not modified) contain a task}
    }
    In all the other case the resulting external proof is considered
    not edited.
*)

(** {2 Transformation} *)

val add_transformation :
  keygen:'key keygen ->
  goal:('goal -> Ident.ident * expl * Task.task) ->
  'key env_session ->
  string ->
  'key goal ->
  'goal list ->
  'key transf
(** Add a transformation by its subgoals *)

val add_registered_transformation :
  keygen:'key keygen ->
  'key env_session ->
  string ->
  'key goal ->
  'key transf
(** Apply a real transformation by its why3 name,
    raise NoTask if the goal doesn't contain a task.
    If the goal already has a transformation with this name,
    it is returned. *)

val remove_transformation : ?notify:'key notify -> 'key transf -> unit
  (** Remove a transformation *)

(** {2 Metas} *)
val add_registered_metas :
  keygen:'key keygen ->
  'key env_session ->
  (string * Theory.meta_arg list) list ->
  'key goal ->
  'key metas
(** Add some metas to a task. If the goal already contain a {!metas}
    with same metas, the old one is returned.
*)

val remove_metas : ?notify:'key notify -> 'key metas -> unit
(** Remove the addition of metas *)

(** {2 File} *)

val add_file :
  keygen:'key keygen ->
  'key env_session ->
  ?format:string ->
  string ->
  'key file
(** Add a real file by its filename. The filename must be relative to
    session_dir *)

val remove_file : 'key file -> unit
(** Remove a file *)

(** {2 Explanation} *)

val get_explanation : Ident.ident -> Task.task -> expl
val goal_expl_task : Task.task -> Ident.ident * expl * Task.task

(** {2 Iterators} *)

(** {3 recursive} *)

val goal_iter_proof_attempt : ('key proof_attempt -> unit) -> 'key goal -> unit
val transf_iter_proof_attempt :
  ('key proof_attempt -> unit) -> 'key transf -> unit
val theory_iter_proof_attempt :
  ('key proof_attempt -> unit) -> 'key theory -> unit
val transf_iter_proof_attempt :
  ('key proof_attempt -> unit) -> 'key transf -> unit
val file_iter_proof_attempt : ('key proof_attempt -> unit) -> 'key file -> unit
val session_iter_proof_attempt :
  ('key proof_attempt -> unit) -> 'key session -> unit
val iter_proof_attempt :
  ('key proof_attempt -> unit) -> 'key any -> unit

val goal_iter_leaf_goal :
  unproved_only:bool -> ('key goal -> unit) -> 'key goal -> unit
(** iter all the goals which are a leaf
    (no transformations are applied on it) *)

(** {3 not recursive} *)

val iter_goal :
  ('key proof_attempt -> unit) ->
  ('key transf -> unit) ->
  ('key metas -> unit) ->
  'key goal -> unit
val iter_transf :
  ('key goal -> unit) -> 'key transf -> unit
val iter_metas :
  ('key goal -> unit) -> 'key metas -> unit


val goal_iter : ('key any -> unit) -> 'key goal -> unit
val transf_iter : ('key any -> unit) -> 'key transf -> unit
val theory_iter : ('key any -> unit) -> 'key theory -> unit
val transf_iter : ('key any -> unit) -> 'key transf -> unit
val metas_iter : ('key any -> unit) -> 'key metas -> unit
val file_iter : ('key any -> unit) -> 'key file -> unit
val session_iter : ('key any -> unit) -> 'key session -> unit
val iter : ('key any -> unit) -> 'key any -> unit

(** {2 Some functorized interface (not very useful...)}*)


(* Claude: if "not very useful" then -> removed
module AddTransf (X : sig
  type key
  val keygen : key keygen

  type goal
  val goal : goal -> Ident.ident * expl * Task.task

  type transf
  val fold_transf : ('a -> goal -> 'a) -> 'a -> Task.task -> transf -> 'a
end) : sig
  val add_transformation : X.key goal -> string -> X.transf -> X.key transf
end
*)

(*
module AddFile(X : sig
  type key
  val keygen : key keygen

  type goal
  val goal : goal -> Ident.ident * expl * Task.task

  type theory
  val fold_theory : ('a -> goal -> 'a) -> 'a -> theory -> 'a

  type file
  val fold_file :
    ('a -> Ident.ident (** thname *) -> theory -> 'a) ->
    'a -> file -> 'a

end) : sig
  val add_file :
    X.key session -> string -> ?format:string -> X.file -> X.key file
end
*)


(*
Local Variables:
compile-command: "unset LANG; make -C ../.. bin/why3ide.byte"
End:
*)
