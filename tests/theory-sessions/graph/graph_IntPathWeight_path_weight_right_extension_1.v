(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint infix_plpl (a:Type)(l1:(list a)) (l2:(list a)) {struct l1}: (list
  a) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.
Unset Implicit Arguments.

Axiom Append_assoc : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall (a:Type), forall (l:(list a)), ((infix_plpl l
  (Nil :(list a))) = l).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint length (a:Type)(l:(list a)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length r))%Z
  end.
Unset Implicit Arguments.

Axiom Length_nonnegative : forall (a:Type), forall (l:(list a)),
  (0%Z <= (length l))%Z.

Axiom Length_nil : forall (a:Type), forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil :(list a))).

Axiom Append_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  ((length (infix_plpl l1 l2)) = ((length l1) + (length l2))%Z).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint mem (a:Type)(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.
Unset Implicit Arguments.

Axiom mem_append : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x l2)).

Axiom mem_decomp : forall (a:Type), forall (x:a) (l:(list a)), (mem x l) ->
  exists l1:(list a), exists l2:(list a), (l = (infix_plpl l1 (Cons x l2))).

Parameter vertex : Type.

Parameter edge: vertex -> vertex -> Prop.

(* Why3 assumption *)
Inductive path : vertex -> (list vertex) -> vertex -> Prop :=
  | Path_empty : forall (x:vertex), (path x (Nil :(list vertex)) x)
  | Path_cons : forall (x:vertex) (y:vertex) (z:vertex) (l:(list vertex)),
      (edge x y) -> ((path y l z) -> (path x (Cons x l) z)).

Axiom path_right_extension : forall (x:vertex) (y:vertex) (z:vertex) (l:(list
  vertex)), (path x l y) -> ((edge y z) -> (path x (infix_plpl l (Cons y
  (Nil :(list vertex)))) z)).

Axiom path_right_inversion : forall (x:vertex) (z:vertex) (l:(list vertex)),
  (path x l z) -> (((x = z) /\ (l = (Nil :(list vertex)))) \/
  exists y:vertex, exists lqt:(list vertex), (path x lqt y) /\ ((edge y z) /\
  (l = (infix_plpl lqt (Cons y (Nil :(list vertex))))))).

Axiom path_trans : forall (x:vertex) (y:vertex) (z:vertex) (l1:(list vertex))
  (l2:(list vertex)), (path x l1 y) -> ((path y l2 z) -> (path x
  (infix_plpl l1 l2) z)).

Axiom empty_path : forall (x:vertex) (y:vertex), (path x (Nil :(list vertex))
  y) -> (x = y).

Parameter weight: vertex -> vertex -> Z.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint path_weight(l:(list vertex)) (dst:vertex) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons x Nil) => (weight x dst)
  | (Cons x ((Cons y _) as r)) => ((weight x y) + (path_weight r dst))%Z
  end.
Unset Implicit Arguments.

Require Import Why3.

(* Why3 goal *)
Theorem path_weight_right_extension : forall (x:vertex) (y:vertex) (l:(list
  vertex)), ((path_weight (infix_plpl l (Cons x (Nil :(list vertex))))
  y) = ((path_weight l x) + (weight x y))%Z).
induction l.
auto.
destruct l.
auto.
replace (infix_plpl (Cons a (Cons v l)) (Cons x Nil))
   with (Cons a (Cons v (infix_plpl l (Cons x Nil)))) by ae.
replace (path_weight (Cons a (Cons v (infix_plpl l (Cons x Nil)))) y)
   with (weight a v + path_weight (Cons v (infix_plpl l (Cons x Nil))) y)%Z
   by ae.
replace (path_weight (Cons a (Cons v l)) x)
   with (weight a v + path_weight (Cons v l) x)%Z by ae.
replace (Cons v (infix_plpl l (Cons x Nil)))
   with (infix_plpl (Cons v l) (Cons x Nil)) by ae.
ae.
Qed.


