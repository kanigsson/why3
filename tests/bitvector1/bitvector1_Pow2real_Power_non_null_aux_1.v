(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.
Require real.Real.
Require real.RealInfix.

Parameter pow2: Z -> R.

Axiom Power_0 : ((pow2 0%Z) = 1%R).

Axiom Power_s : forall (n:Z), (0%Z <= n)%Z ->
  ((pow2 (n + 1%Z)%Z) = (2%R * (pow2 n))%R).

Axiom Power_p : forall (n:Z), (n <= 0%Z)%Z ->
  ((pow2 (n - 1%Z)%Z) = ((05 / 10)%R * (pow2 n))%R).

Axiom Power_s_all : forall (n:Z), ((pow2 (n + 1%Z)%Z) = (2%R * (pow2 n))%R).

Axiom Power_p_all : forall (n:Z),
  ((pow2 (n - 1%Z)%Z) = ((05 / 10)%R * (pow2 n))%R).

Axiom Power_1_2 : ((05 / 10)%R = (Rdiv 1%R 2%R)%R).

Axiom Power_1 : ((pow2 1%Z) = 2%R).

Axiom Power_neg1 : ((pow2 (-1%Z)%Z) = (05 / 10)%R).

Open Scope Z_scope.

(* Why3 goal *)
Theorem Power_non_null_aux : forall (n:Z), (0%Z <= n)%Z ->
  ~ ((pow2 n) = 0%R).
(* YOU MAY EDIT THE PROOF BELOW *)
intros n H. 
cut (0 <= n); auto with zarith.
apply Z_lt_induction with
  (P:= fun n => 
       0 <= n -> (pow2 n <> 0)%R);auto with zarith.
intros x Hind Hxpos.
assert (hx:x = 0 \/ x >0) by omega.
destruct hx.
subst x.
rewrite Power_0;auto with *.
(*x>0*)
replace (x) with (x-1+1) by omega.
rewrite Power_s;auto with *.
apply Rmult_integral_contrapositive.
split.
apply Rgt_not_eq;auto with *.
apply Hind;auto with *.

Qed.


