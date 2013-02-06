(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.
Definition unit  := unit.

Parameter qtmark : Type.

Parameter at1: forall (a:Type), a -> qtmark -> a.

Implicit Arguments at1.

Parameter old: forall (a:Type), a -> a.

Implicit Arguments old.

Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

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
  ((length l) = 0%Z) <-> (l = (Nil:(list a))).

Inductive sorted : (list Z) -> Prop :=
  | Sorted_Nil : (sorted (Nil:(list Z)))
  | Sorted_One : forall (x:Z), (sorted (Cons x (Nil:(list Z))))
  | Sorted_Two : forall (x:Z) (y:Z) (l:(list Z)), (x <= y)%Z ->
      ((sorted (Cons y l)) -> (sorted (Cons x (Cons y l)))).

Set Implicit Arguments.
Fixpoint mem (a:Type)(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.
Unset Implicit Arguments.

Axiom sorted_mem : forall (x:Z) (l:(list Z)), ((forall (y:Z), (mem y l) ->
  (x <= y)%Z) /\ (sorted l)) <-> (sorted (Cons x l)).

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
  (Nil:(list a))) = l).

Axiom Append_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  ((length (infix_plpl l1 l2)) = ((length l1) + (length l2))%Z).

Axiom mem_append : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x l2)).

Axiom mem_decomp : forall (a:Type), forall (x:a) (l:(list a)), (mem x l) ->
  exists l1:(list a), exists l2:(list a), (l = (infix_plpl l1 (Cons x l2))).

Parameter num_occ: forall (a:Type), a -> (list a) -> Z.

Implicit Arguments num_occ.

Axiom num_occ_def : forall (a:Type), forall (x:a) (l:(list a)),
  match l with
  | Nil => ((num_occ x l) = 0%Z)
  | (Cons y r) => ((x = y) -> ((num_occ x l) = (1%Z + (num_occ x r))%Z)) /\
      ((~ (x = y)) -> ((num_occ x l) = (0%Z + (num_occ x r))%Z))
  end.

Axiom Mem_Num_Occ : forall (a:Type), forall (x:a) (l:(list a)), (mem x l) <->
  (0%Z <  (num_occ x l))%Z.

Axiom Append_Num_Occ : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list
  a)), ((num_occ x (infix_plpl l1 l2)) = ((num_occ x l1) + (num_occ x
  l2))%Z).

Definition permut (a:Type)(l1:(list a)) (l2:(list a)): Prop := forall (x:a),
  ((num_occ x l1) = (num_occ x l2)).
Implicit Arguments permut.

Axiom Permut_refl : forall (a:Type), forall (l:(list a)), (permut l l).

Axiom Permut_sym : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  (permut l1 l2) -> (permut l2 l1).

Axiom Permut_trans : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), (permut l1 l2) -> ((permut l2 l3) -> (permut l1 l3)).

Axiom Permut_cons : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list
  a)), (permut l1 l2) -> (permut (Cons x l1) (Cons x l2)).

Axiom Permut_swap : forall (a:Type), forall (x:a) (y:a) (l:(list a)),
  (permut (Cons x (Cons y l)) (Cons y (Cons x l))).

Axiom Permut_mem : forall (a:Type), forall (x:a) (l1:(list a)) (l2:(list a)),
  (permut l1 l2) -> ((mem x l1) -> (mem x l2)).

Axiom Permut_length : forall (a:Type), forall (l1:(list a)) (l2:(list a)),
  (permut l1 l2) -> ((length l1) = (length l2)).

Axiom Permut_cons_append : forall (a:Type), forall (x:a) (l1:(list a))
  (l2:(list a)), (permut (infix_plpl (Cons x l1) l2) (infix_plpl l1 (Cons x
  l2))).

Axiom Permut_assoc : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (l3:(list a)), (permut (infix_plpl (infix_plpl l1 l2) l3) (infix_plpl l1
  (infix_plpl l2 l3))).

Axiom Permut_append : forall (a:Type), forall (l1:(list a)) (l2:(list a))
  (k1:(list a)) (k2:(list a)), (permut l1 k1) -> ((permut l2 k2) ->
  (permut (infix_plpl l1 l2) (infix_plpl k1 k2))).

Axiom Permut_append_swap : forall (a:Type), forall (l1:(list a)) (l2:(list
  a)), (permut (infix_plpl l1 l2) (infix_plpl l2 l1)).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Theorem WP_parameter_merge : forall (l1:(list Z)), forall (l2:(list Z)),
  ((sorted l1) /\ (sorted l2)) ->
  match l2 with
  | (Cons x x1) =>
      match l1 with
      | (Cons x2 x3) => (~ (x2 <= x)%Z) ->
          ((((0%Z <= ((length l1) + (length l2))%Z)%Z /\
          (((length l1) + (length x1))%Z <  ((length l1) + (length l2))%Z)%Z) /\
          ((sorted l1) /\ (sorted x1))) -> forall (result:(list Z)),
          ((sorted result) /\ (permut result (infix_plpl l1 x1))) ->
          (permut (Cons x result) (infix_plpl l1 l2)))
      | Nil => True
      end
  | Nil => True
  end.
(* YOU MAY EDIT THE PROOF BELOW *)
intuition.
destruct l2; intuition.
destruct l1; intuition.
apply Permut_trans with (Cons z (infix_plpl (Cons z0 l1) l2)); auto.
apply Permut_cons; auto.
apply Permut_cons_append.
Qed.
(* DO NOT EDIT BELOW *)

