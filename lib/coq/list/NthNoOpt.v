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

(* Why3 goal *)
Definition nth {a:Type} {a_WT:WhyType a} : Z -> (list a) -> a.
exact (fix nth n l := match l with nil => why_inhabitant | cons h t => if Zeq_bool n Z0 then h else nth (n - 1)%Z t end).
Defined.

(* Why3 goal *)
Lemma nth_cons_0 {a:Type} {a_WT:WhyType a} :
  forall (x:a) (r:(list a)), ((nth 0%Z (Init.Datatypes.cons x r)) = x).
Proof.
now intros x r.
Qed.

(* Why3 goal *)
Lemma nth_cons_n {a:Type} {a_WT:WhyType a} :
  forall (x:a) (r:(list a)) (n:Z), (0%Z < n)%Z ->
  ((nth n (Init.Datatypes.cons x r)) = (nth (n - 1%Z)%Z r)).
Proof.
intros x r n h1.
simpl.
generalize (Zeq_bool_if n 0).
case Zeq_bool ; try easy.
now intros ->.
Qed.

