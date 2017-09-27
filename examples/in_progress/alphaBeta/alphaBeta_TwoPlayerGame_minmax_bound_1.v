(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Implicit Arguments Nil [[a]].
Implicit Arguments Cons [[a]].

Parameter position : Type.

Parameter move : Type.

Parameter initial_position: position.

Parameter legal_moves: position -> (list move).

Parameter do_move: position -> move -> position.

Parameter winning_value: Z.

Parameter infinity: Z.

Parameter position_value: position -> Z.

Axiom position_value_bound : forall (p:position),
  ((-infinity)%Z < (position_value p))%Z /\
  ((position_value p) < infinity)%Z.

Parameter minmax: position -> Z -> Z.

Axiom minmax_depth_0 : forall (p:position), ((minmax p
  0%Z) = (position_value p)).

(* Why3 assumption *)
Definition param  := (position* Z)%type.

(* Why3 assumption *)
Definition cost(p:(position* Z)%type) (m:move): Z :=
  match p with
  | (p1, n) => (minmax (do_move p1 m) n)
  end.

Parameter set : forall (a:Type), Type.

Parameter mem: forall {a:Type}, a -> (set a) -> Prop.

(* Why3 assumption *)
Definition infix_eqeq {a:Type}(s1:(set a)) (s2:(set a)): Prop :=
  forall (x:a), (mem x s1) <-> (mem x s2).

Axiom extensionality : forall {a:Type}, forall (s1:(set a)) (s2:(set a)),
  (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset {a:Type}(s1:(set a)) (s2:(set a)): Prop := forall (x:a),
  (mem x s1) -> (mem x s2).

Axiom subset_trans : forall {a:Type}, forall (s1:(set a)) (s2:(set a))
  (s3:(set a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1 s3)).

Parameter empty: forall {a:Type}, (set a).

(* Why3 assumption *)
Definition is_empty {a:Type}(s:(set a)): Prop := forall (x:a), ~ (mem x s).

Axiom empty_def1 : forall {a:Type}, (is_empty (empty :(set a))).

Parameter add: forall {a:Type}, a -> (set a) -> (set a).

Axiom add_def1 : forall {a:Type}, forall (x:a) (y:a), forall (s:(set a)),
  (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall {a:Type}, a -> (set a) -> (set a).

Axiom remove_def1 : forall {a:Type}, forall (x:a) (y:a) (s:(set a)), (mem x
  (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall {a:Type}, forall (x:a) (s:(set a)),
  (subset (remove x s) s).

Parameter union: forall {a:Type}, (set a) -> (set a) -> (set a).

Axiom union_def1 : forall {a:Type}, forall (s1:(set a)) (s2:(set a)) (x:a),
  (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall {a:Type}, (set a) -> (set a) -> (set a).

Axiom inter_def1 : forall {a:Type}, forall (s1:(set a)) (s2:(set a)) (x:a),
  (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall {a:Type}, (set a) -> (set a) -> (set a).

Axiom diff_def1 : forall {a:Type}, forall (s1:(set a)) (s2:(set a)) (x:a),
  (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall {a:Type}, forall (s1:(set a)) (s2:(set a)),
  (subset (diff s1 s2) s1).

Parameter choose: forall {a:Type}, (set a) -> a.

Axiom choose_def : forall {a:Type}, forall (s:(set a)), (~ (is_empty s)) ->
  (mem (choose s) s).

Parameter cardinal: forall {a:Type}, (set a) -> Z.

Axiom cardinal_nonneg : forall {a:Type}, forall (s:(set a)),
  (0%Z <= (cardinal s))%Z.

Axiom cardinal_empty : forall {a:Type}, forall (s:(set a)),
  ((cardinal s) = 0%Z) <-> (is_empty s).

Axiom cardinal_add : forall {a:Type}, forall (x:a), forall (s:(set a)),
  (~ (mem x s)) -> ((cardinal (add x s)) = (1%Z + (cardinal s))%Z).

Axiom cardinal_remove : forall {a:Type}, forall (x:a), forall (s:(set a)),
  (mem x s) -> ((cardinal s) = (1%Z + (cardinal (remove x s)))%Z).

Axiom cardinal_subset : forall {a:Type}, forall (s1:(set a)) (s2:(set a)),
  (subset s1 s2) -> ((cardinal s1) <= (cardinal s2))%Z.

Axiom cardinal1 : forall {a:Type}, forall (s:(set a)),
  ((cardinal s) = 1%Z) -> forall (x:a), (mem x s) -> (x = (choose s)).

Parameter nth: forall {a:Type}, Z -> (set a) -> a.

Axiom nth_injective : forall {a:Type}, forall (s:(set a)) (i:Z) (j:Z),
  ((0%Z <= i)%Z /\ (i < (cardinal s))%Z) -> (((0%Z <= j)%Z /\
  (j < (cardinal s))%Z) -> (((nth i s) = (nth j s)) -> (i = j))).

Axiom nth_surjective : forall {a:Type}, forall (s:(set a)) (x:a), (mem x
  s) -> exists i:Z, ((0%Z <= i)%Z /\ (i < (cardinal s))%Z) -> (x = (nth i
  s)).

Parameter min: (position* Z)%type -> (set move) -> Z.

Axiom min_is_a_lower_bound : forall (p:(position* Z)%type) (s:(set move))
  (x:move), (mem x s) -> ((min p s) <= (cost p x))%Z.

Axiom min_appears_in_set : forall (p:(position* Z)%type) (s:(set move)),
  (~ (is_empty s)) -> exists x:move, (mem x s) /\ ((cost p x) = (min p s)).

(* Why3 assumption *)
Fixpoint mem1 {a:Type}(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem1 x r)
  end.

Parameter elements: forall {a:Type}, (list a) -> (set a).

Axiom elements_mem : forall {a:Type}, forall (l:(list a)) (x:a), (mem1 x
  l) <-> (mem x (elements l)).

Axiom elements_Nil : forall {a:Type}, ((elements (Nil :(list
  a))) = (empty :(set a))).

Axiom minmax_depth_non_zero : forall (p:position) (n:Z), (0%Z < n)%Z ->
  let moves := (elements (legal_moves p)) in (((is_empty moves) -> ((minmax p
  n) = (position_value p))) /\ ((~ (is_empty moves)) -> ((minmax p
  n) = (-(min (p, (n - 1%Z)%Z) moves))%Z))).

Open Scope Z_scope.
Require Import Why3.

Ltac ae := why3 "alt-ergo" timelimit 3.

(* Why3 goal *)
Theorem minmax_bound : forall (p:position) (d:Z), (0%Z <= d)%Z ->
  (((-infinity)%Z < (minmax p d))%Z /\ ((minmax p d) < infinity)%Z).
intros p d h1.
generalize p h1; clear p.
pattern d; apply Z_lt_induction; auto.
clear d h1.
intros d Hind p Hdpos.
assert (h:d = 0 \/ 0 < d) by omega.
destruct h.
ae.
generalize (minmax_depth_non_zero p d H).
set (moves := elements (legal_moves p)).
intros (H1 & H2).
assert (h:is_empty moves \/ ~ (is_empty moves)) by ae.
destruct h.
ae.
rewrite H2; auto.
generalize (min_appears_in_set (p,d-1) moves H0).
intros (m & h1 & h2).
rewrite <- h2.
unfold cost.
assert (h: 0 <= d-1 < d) by omega.
generalize (Hind (d-1) h (do_move p m)).
ae.
Qed.


