(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.

Require Import floating_point.GenFloat.

(* Why3 goal *)
Definition double : Type.
exact (t 53 1024).
Defined.

Global Instance double_WhyType : WhyType double.
Proof.
apply t_WhyType.
Qed.
