(* This file is generated by Why3's Coq-realize driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.

Require Import ClassicalEpsilon.

Inductive _map (a b:Type) :=
  | _map_constr : (a -> b) -> _map a b.

(* Why3 goal *)
Definition map : forall (a:Type) {a_WT:WhyType a} (b:Type) {b_WT:WhyType b},
  Type.
intros.
exact (_map a b).
Defined.

Global Instance map_WhyType : forall (a:Type) {a_WT:WhyType a} (b:Type) {b_WT:WhyType b}, WhyType (map a b).
Proof.
intros.
repeat split.
exact (fun _ => why_inhabitant).
intros x y.
apply excluded_middle_informative.
Qed.

(* Why3 goal *)
Definition get: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b.
intros a a_WT b b_WT (m) x.
exact (m x).
Defined.

(* Why3 goal *)
Definition set: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b -> (map a b).
intros a a_WT b b_WT (m) x y.
split.
intros x'.
destruct (why_decidable_eq x x') as [H|H].
exact y.
exact (m x').
Defined.

(* Why3 goal *)
Lemma Select_eq : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (m:(map a b)), forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) ->
  ((get (set m a1 b1) a2) = b1).
Proof.
intros a a_WT b b_WT (m) a1 a2 b1 h1.
unfold get, set.
now case why_decidable_eq.
Qed.

(* Why3 goal *)
Lemma Select_neq : forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, forall (m:(map a b)), forall (a1:a) (a2:a),
  forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1) a2) = (get m a2)).
Proof.
intros a a_WT b b_WT (m) a1 a2 b1 h1.
unfold get, set.
now case why_decidable_eq.
Qed.

(* Why3 goal *)
Definition const: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  b -> (map a b).
intros a a_WT b b_WT y.
exact (_map_constr _ _ (fun _ => y)).
Defined.

(* Why3 goal *)
Lemma Const : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (b1:b) (a1:a), ((get (const b1:(map a b)) a1) = b1).
Proof.
easy.
Qed.

