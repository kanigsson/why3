(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.

Global Instance option_WhyType : forall T {T_WT : WhyType T}, WhyType (option T).
split.
apply @None.
intros [x|] [y|] ; try (now right) ; try (now left).
destruct (why_decidable_eq x y) as [E|E].
left.
now apply f_equal.
right.
contradict E.
now injection E.
Qed.
