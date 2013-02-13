(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.

(* Why3 assumption *)
Definition unit  := unit.

(* Why3 assumption *)
Inductive list (a:Type) {a_WT:WhyType a} :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Axiom list_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (list a).
Existing Instance list_WhyType.
Implicit Arguments Nil [[a] [a_WT]].
Implicit Arguments Cons [[a] [a_WT]].

Parameter head: forall {a:Type} {a_WT:WhyType a}, (list a) -> a.

Axiom head_cons : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(list
  a)), ((head (Cons x l)) = x).

Parameter tail: forall {a:Type} {a_WT:WhyType a}, (list a) -> (list a).

Axiom tail_cons : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(list
  a)), ((tail (Cons x l)) = l).

(* Why3 assumption *)
Fixpoint mem {a:Type} {a_WT:WhyType a}(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.

(* Why3 assumption *)
Definition disjoint {a:Type} {a_WT:WhyType a}(l1:(list a)) (l2:(list
  a)): Prop := forall (x:a), ~ ((mem x l1) /\ (mem x l2)).

(* Why3 assumption *)
Fixpoint no_repet {a:Type} {a_WT:WhyType a}(l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => True
  | (Cons x r) => (~ (mem x r)) /\ (no_repet r)
  end.

(* Why3 assumption *)
Fixpoint infix_plpl {a:Type} {a_WT:WhyType a}(l1:(list a)) (l2:(list
  a)) {struct l1}: (list a) :=
  match l1 with
  | Nil => l2
  | (Cons x1 r1) => (Cons x1 (infix_plpl r1 l2))
  end.

Axiom Append_assoc : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (l3:(list a)), ((infix_plpl l1 (infix_plpl l2
  l3)) = (infix_plpl (infix_plpl l1 l2) l3)).

Axiom Append_l_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((infix_plpl l (Nil :(list a))) = l).

(* Why3 assumption *)
Fixpoint length {a:Type} {a_WT:WhyType a}(l:(list a)) {struct l}: Z :=
  match l with
  | Nil => 0%Z
  | (Cons _ r) => (1%Z + (length r))%Z
  end.

Axiom Length_nonnegative : forall {a:Type} {a_WT:WhyType a}, forall (l:(list
  a)), (0%Z <= (length l))%Z.

Axiom Length_nil : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((length l) = 0%Z) <-> (l = (Nil :(list a))).

Axiom Append_length : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)), ((length (infix_plpl l1
  l2)) = ((length l1) + (length l2))%Z).

Axiom mem_append : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l1:(list
  a)) (l2:(list a)), (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x
  l2)).

Axiom mem_decomp : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(list
  a)), (mem x l) -> exists l1:(list a), exists l2:(list a),
  (l = (infix_plpl l1 (Cons x l2))).

(* Why3 assumption *)
Fixpoint reverse {a:Type} {a_WT:WhyType a}(l:(list a)) {struct l}: (list
  a) :=
  match l with
  | Nil => (Nil :(list a))
  | (Cons x r) => (infix_plpl (reverse r) (Cons x (Nil :(list a))))
  end.

Axiom reverse_append : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)) (x:a), ((infix_plpl (reverse (Cons x l1))
  l2) = (infix_plpl (reverse l1) (Cons x l2))).

Axiom reverse_reverse : forall {a:Type} {a_WT:WhyType a}, forall (l:(list
  a)), ((reverse (reverse l)) = l).

Axiom Reverse_length : forall {a:Type} {a_WT:WhyType a}, forall (l:(list a)),
  ((length (reverse l)) = (length l)).

Axiom loc : Type.
Parameter loc_WhyType : WhyType loc.
Existing Instance loc_WhyType.

Parameter null: loc.

(* Why3 assumption *)
Inductive list_seg : loc -> (map.Map.map loc loc) -> (list loc)
  -> loc -> Prop :=
  | list_seg_nil : forall (p:loc) (next:(map.Map.map loc loc)), (list_seg p
      next (Nil :(list loc)) p)
  | list_seg_cons : forall (p:loc) (q:loc) (next:(map.Map.map loc loc))
      (l:(list loc)), ((~ (p = null)) /\ (list_seg (map.Map.get next p) next
      l q)) -> (list_seg p next (Cons p l) q).



(* Why3 goal *)
Theorem list_seg_frame : forall (next1:(map.Map.map loc loc))
  (next2:(map.Map.map loc loc)) (p:loc) (q:loc) (v:loc) (pM:(list loc)),
  ((list_seg p next1 pM null) /\ ((next2 = (map.Map.set next1 q v)) /\
  ~ (mem q pM))) -> (list_seg p next2 pM null).
Proof.
intros until pM.
generalize p; clear p.
elim pM.
(* case pM = Nil *)
intros p (h1&h2&h3).
inversion h1; subst; clear h1.
constructor.
(* case pM = Cons a l *)
intros a l Hind p (h1&h2&h3).
inversion h1; subst; clear h1.
destruct H4.
constructor; split; auto.
simpl in h3.
rewrite Map.Select_neq; auto.
apply Hind; split; auto.
Qed.

