(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require list.Nth.
Require option.Option.
Require list.NthLength.
Require list.Append.
Require list.NthLengthAppend.

(* Why3 assumption *)
Definition unit := unit.

Axiom qtmark : Type.
Parameter qtmark_WhyType : WhyType qtmark.
Existing Instance qtmark_WhyType.

(* Why3 assumption *)
Definition pal {a:Type} {a_WT:WhyType a} (x:(list a)) (n:Z): Prop :=
  forall (i:Z), ((0%Z <= i)%Z /\ (i < n)%Z) -> ((list.Nth.nth i
  x) = (list.Nth.nth ((n - 1%Z)%Z - i)%Z x)).

(* Why3 goal *)
Theorem WP_parameter_palindrome_rec : forall {a:Type} {a_WT:WhyType a},
  forall (x:(list a)) (y:(list a)),
  ((list.Length.length y) <= (list.Length.length x))%Z -> forall (x1:a)
  (x2:(list a)), (y = (Init.Datatypes.cons x1 x2)) -> forall (x3:a)
  (x4:(list a)), (x2 = (Init.Datatypes.cons x3 x4)) -> forall (x5:a)
  (x6:(list a)), (x = (Init.Datatypes.cons x5 x6)) ->
  (((list.Length.length x4) <= (list.Length.length x6))%Z -> ((exists i:Z,
  ((0%Z <= i)%Z /\ (i < (list.Length.length x4))%Z) /\ ~ ((list.Nth.nth i
  x6) = (list.Nth.nth (((list.Length.length x4) - 1%Z)%Z - i)%Z x6))) ->
  exists i:Z, ((0%Z <= i)%Z /\ (i < (list.Length.length y))%Z) /\
  ~ ((list.Nth.nth i
  x) = (list.Nth.nth (((list.Length.length y) - 1%Z)%Z - i)%Z x)))).
(* Why3 intros a a_WT x y h1 x1 x2 h2 x3 x4 h3 x5 x6 h4 h5 (i,((h6,h7),h8)). *)
intros a a_WT x y h1 x1 x2 h2 x3 x4 h3 x5 x6 h4 hl (i,(hi1,hi2)).
subst.
exists (i+1)%Z; intuition.
unfold Length.length. fold Length.length.
omega.
unfold Length.length in *. fold Length.length in *.
assert (Nth.nth (i+1) (x5 :: x6) = Nth.nth i x6).
  unfold Nth.nth; fold Nth.nth.
  generalize (Zeq_bool_eq (i+1) 0).
  destruct (Zeq_bool (i+1) 0).
  intuition.
  elimtype False.
  omega.
  intuition.
  replace (i+1-1)%Z with i by omega. auto.
replace (1 + (1 + Length.length x4) - 1 - (i + 1))%Z
         with (1 + Length.length x4 - 1 - i)%Z
  in H1 by omega.
assert (Nth.nth (1 + Length.length x4 - 1 - i) (x5 :: x6) =
        Nth.nth (Length.length x4 - 1 - i) x6).
    unfold Nth.nth; fold Nth.nth.
  generalize (Zeq_bool_eq (1 + Length.length x4 - 1 - i) 0).
    destruct (Zeq_bool (1 + Length.length x4 - 1 - i) 0).
  intuition; elimtype False; omega.
  intuition.
  replace (1 + Length.length x4 - 1 - i - 1)%Z with (Length.length x4 - 1 - i)%Z
  by omega; auto.
congruence.
Qed.

