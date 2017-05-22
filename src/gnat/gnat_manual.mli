open Why3

type goal = Session_itp.proofNodeID
type attempt = Session_itp.proofAttemptID

val manual_attempt_of_goal : Session_itp.session -> goal -> attempt option

val is_new_manual_proof : Session_itp.session -> goal -> bool

(* Create a new file with the goal to be proved and
   adds an external proof. Returns the name of the created file *)
(* TODO to be redone
val create_prover_file :
  goal -> Gnat_expl.check -> Session.loaded_prover -> attempt

(* Get the file associated to the goal with the current manual prover.
   If the current prover is not manual None is returned *)
val manual_proof_info : attempt -> (string * string) option
*)
