(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2018   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require int.Abs.
Require int.EuclideanDivision.
Require int.ComputerDivision.
Require number.Parity.
Require number.Divisibility.

Import Znumtheory.

(* Why3 assumption *)
Definition prime (p:Z) : Prop :=
  (2%Z <= p)%Z /\
  forall (n:Z), ((1%Z < n)%Z /\ (n < p)%Z) ->
  ~ (number.Divisibility.divides n p).

Lemma prime_is_Zprime :
  forall p, prime p <-> Znumtheory.prime p.
Proof.
intros p.
apply iff_trans with (2 := prime_alt p).
unfold prime, prime'.
intuition.
Qed.

(* Why3 goal *)
Lemma not_prime_1 : ~ (prime 1%Z).
intros (H1,_).
now elim H1.
Qed.

(* Why3 goal *)
Lemma prime_2 : prime 2%Z.
Proof.
apply <- prime_is_Zprime.
apply prime_2.
Qed.

(* Why3 goal *)
Lemma prime_3 : prime 3%Z.
Proof.
apply <- prime_is_Zprime.
apply prime_3.
Qed.

(* Why3 goal *)
Lemma prime_divisors :
  forall (p:Z), (prime p) -> forall (d:Z),
  (number.Divisibility.divides d p) ->
  (d = 1%Z) \/ ((d = (-1%Z)%Z) \/ ((d = p) \/ (d = (-p)%Z))).
Proof.
intros p Hp d Hd.
apply -> prime_is_Zprime in Hp.
destruct (prime_divisors p Hp d Hd) ; intuition.
Qed.

(* Why3 goal *)
Lemma small_divisors :
  forall (p:Z), (2%Z <= p)%Z ->
  (forall (d:Z), (2%Z <= d)%Z -> (prime d) ->
   ((1%Z < (d * d)%Z)%Z /\ ((d * d)%Z <= p)%Z) ->
   ~ (number.Divisibility.divides d p)) ->
  prime p.
Proof.
intros p Hp H.
apply <- prime_is_Zprime.
destruct (prime_dec p) as [Pp|Pp].
exact Pp.
elimtype False.
(* *)
assert (exists d, (2 <= d)%Z /\ (d * d <= p)%Z /\ prime d /\ Zdivide d p).
clear H.
assert (Hp' : (0 <= p)%Z) by omega.
revert p Hp' Hp Pp.
apply (Zlt_0_ind (fun p => 2 <= p -> ~ Znumtheory.prime p -> (exists d : Z, 2 <= d /\ d * d <= p /\ prime d /\ (d | p)))%Z).
intros p IH _ Hp Pp.
destruct (not_prime_divide p) as (x,(Hx1,Hx2)).
clear -Hp ; omega.
exact Pp.
destruct (Zle_or_lt (x * x) p) as [Hx|Hx].
destruct (prime_dec x) as [Px|Px].
exists x.
split.
clear -Hx1 ; omega.
split.
exact Hx.
split.
now apply <- prime_is_Zprime.
exact Hx2.
destruct (IH x) as (y&Hy1&Hy2&Hy3&Hy4).
clear -Hx1 ; omega.
clear -Hx1 ; omega.
exact Px.
exists y.
refine (conj Hy1 (conj _ (conj Hy3 _))).
apply Zle_trans with (1 := Hy2).
apply Zle_trans with (2 := Hx).
rewrite <- (Zmult_1_r x) at 1.
apply Zmult_le_compat_l.
now apply Zlt_le_weak.
clear -Hx1 ; omega.
now apply Zdivide_trans with x.
case Hx2.
intros q Hq1.
assert (Hq2 : (2 <= q)%Z).
apply (Zlt_le_succ 1).
apply Zmult_lt_reg_r with x.
clear -Hx1 ; omega.
now rewrite Zmult_1_l, <- Hq1.
destruct (prime_dec q) as [Pq|Pq].
exists q.
split.
exact Hq2.
split.
rewrite Hq1.
apply Zmult_le_compat_l.
apply Zlt_le_weak.
apply Zmult_lt_reg_r with x.
clear -Hx1 ; omega.
now rewrite <- Hq1.
clear -Hq2 ; omega.
split.
now apply <- prime_is_Zprime.
exists x.
now rewrite Zmult_comm.
destruct (IH q) as (y&Hy1&Hy2&Hy3&Hy4).
split.
clear -Hq2 ; omega.
rewrite <- (Zmult_1_r q), Hq1.
apply Zmult_lt_compat_l.
clear -Hq2 ; omega.
clear -Hx1 ; omega.
exact Hq2.
exact Pq.
exists y.
refine (conj Hy1 (conj _ (conj Hy3 _))).
apply Zle_trans with (1 := Hy2).
rewrite <- (Zmult_1_r q), Hq1.
apply Zmult_le_compat_l.
clear -Hx1 ; omega.
clear -Hq2 ; omega.
apply Zdivide_trans with (1 := Hy4).
exists x.
now rewrite Zmult_comm.
destruct H0 as (y&Hy1&Hy2&Hy3&Hy4).
apply (H y) ; try easy.
split.
apply Zlt_le_trans with (2 * 2)%Z.
easy.
now apply Zmult_le_compat.
exact Hy2.
Qed.

(* Why3 goal *)
Lemma even_prime :
  forall (p:Z), (prime p) -> (number.Parity.even p) -> (p = 2%Z).
Proof.
intros p Pp (q,Hq).
generalize (proj2 Pp q).
assert (Zdivide q p).
now exists 2%Z.
intros.
refine (_ (fun H1 => H0 H1 H) (proj1 Pp)).
(* ??? omega fails to solve this goal ??? *)
clear -Hq.
intros H Hp.
destruct (Zle_lt_or_eq 2 p Hp) as [Hp'|Hp'].
elim H.
omega.
easy.
Qed.

(* Why3 goal *)
Lemma odd_prime :
  forall (p:Z), (prime p) -> (3%Z <= p)%Z -> number.Parity.odd p.
Proof.
intros p Pp Hp.
apply <- Divisibility.odd_divides.
apply proj2 in Pp.
apply Pp.
omega.
Qed.

