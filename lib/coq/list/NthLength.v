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
Require list.List.
Require list.Length.
Require list.Nth.
Require option.Option.

(* Why3 goal *)
Lemma nth_none_1 :
forall {a:Type} {a_WT:WhyType a},
forall (l:(list a)) (i:Z),
 (i < 0%Z)%Z -> ((list.Nth.nth i l) = Init.Datatypes.None).
Proof.
intros a a_WT l.
induction l as [|h q].
easy.
intros i H.
simpl.
generalize (Zeq_bool_if i 0).
case Zeq_bool.
intros H'.
now rewrite H' in H.
intros _.
apply IHq.
omega.
Qed.

(* Why3 goal *)
Lemma nth_none_2 :
forall {a:Type} {a_WT:WhyType a},
forall (l:(list a)) (i:Z),
 ((list.Length.length l) <= i)%Z ->
 ((list.Nth.nth i l) = Init.Datatypes.None).
Proof.
intros a a_WT l.
induction l as [|h q].
easy.
intros i H.
unfold Length.length in H.
fold Length.length in H.
simpl.
generalize (Zeq_bool_if i 0).
case Zeq_bool.
intros H'.
rewrite H' in H.
exfalso.
generalize (Length.Length_nonnegative q).
omega.
intros _.
apply IHq.
omega.
Qed.

(* Why3 goal *)
Lemma nth_none_3 :
forall {a:Type} {a_WT:WhyType a},
forall (l:(list a)) (i:Z),
 ((list.Nth.nth i l) = Init.Datatypes.None) ->
 ((i < 0%Z)%Z \/ ((list.Length.length l) <= i)%Z).
Proof.
intros a a_WT l.
induction l as [|h q].
intros i _.
simpl.
omega.
intros i.
simpl (Nth.nth i (h :: q)).
change (Length.length (h :: q)) with (1 + Length.length q)%Z.
generalize (Zeq_bool_if i 0).
case Zeq_bool.
easy.
intros Hi H.
specialize (IHq _ H).
omega.
Qed.

