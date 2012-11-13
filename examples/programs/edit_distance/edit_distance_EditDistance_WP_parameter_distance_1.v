(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require int.MinMax.

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

Axiom char : Type.
Parameter char_WhyType : WhyType char.
Existing Instance char_WhyType.

(* Why3 assumption *)
Definition word  := (list char).

(* Why3 assumption *)
Inductive dist : (list char) -> (list char) -> Z -> Prop :=
  | dist_eps : (dist (Nil :(list char)) (Nil :(list char)) 0%Z)
  | dist_add_left : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist (Cons a w1) w2 (n + 1%Z)%Z)
  | dist_add_right : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist w1 (Cons a w2) (n + 1%Z)%Z)
  | dist_context : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
      w2 n) -> forall (a:char), (dist (Cons a w1) (Cons a w2) n).

(* Why3 assumption *)
Definition min_dist(w1:(list char)) (w2:(list char)) (n:Z): Prop := (dist w1
  w2 n) /\ forall (m:Z), (dist w1 w2 m) -> (n <= m)%Z.

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

Axiom Append_length : forall {a:Type} {a_WT:WhyType a}, forall (l1:(list a))
  (l2:(list a)), ((length (infix_plpl l1
  l2)) = ((length l1) + (length l2))%Z).

(* Why3 assumption *)
Fixpoint mem {a:Type} {a_WT:WhyType a}(x:a) (l:(list a)) {struct l}: Prop :=
  match l with
  | Nil => False
  | (Cons y r) => (x = y) \/ (mem x r)
  end.

Axiom mem_append : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l1:(list
  a)) (l2:(list a)), (mem x (infix_plpl l1 l2)) <-> ((mem x l1) \/ (mem x
  l2)).

Axiom mem_decomp : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (l:(list
  a)), (mem x l) -> exists l1:(list a), exists l2:(list a),
  (l = (infix_plpl l1 (Cons x l2))).

(* Why3 assumption *)
Fixpoint last_char(a:char) (u:(list char)) {struct u}: char :=
  match u with
  | Nil => a
  | (Cons c u') => (last_char c u')
  end.

(* Why3 assumption *)
Fixpoint but_last(a:char) (u:(list char)) {struct u}: (list char) :=
  match u with
  | Nil => (Nil :(list char))
  | (Cons c u') => (Cons a (but_last c u'))
  end.

Axiom first_last_explicit : forall (u:(list char)) (a:char),
  ((infix_plpl (but_last a u) (Cons (last_char a u) (Nil :(list
  char)))) = (Cons a u)).

Axiom first_last : forall (a:char) (u:(list char)), exists v:(list char),
  exists b:char, ((infix_plpl v (Cons b (Nil :(list char)))) = (Cons a u)) /\
  ((length v) = (length u)).

Axiom key_lemma_right : forall (w1:(list char)) (w'2:(list char)) (m:Z)
  (a:char), (dist w1 w'2 m) -> forall (w2:(list char)), (w'2 = (Cons a
  w2)) -> exists u1:(list char), exists v1:(list char), exists k:Z,
  (w1 = (infix_plpl u1 v1)) /\ ((dist v1 w2 k) /\
  ((k + (length u1))%Z <= (m + 1%Z)%Z)%Z).

Axiom dist_symetry : forall (w1:(list char)) (w2:(list char)) (n:Z), (dist w1
  w2 n) -> (dist w2 w1 n).

Axiom key_lemma_left : forall (w1:(list char)) (w2:(list char)) (m:Z)
  (a:char), (dist (Cons a w1) w2 m) -> exists u2:(list char), exists v2:(list
  char), exists k:Z, (w2 = (infix_plpl u2 v2)) /\ ((dist w1 v2 k) /\
  ((k + (length u2))%Z <= (m + 1%Z)%Z)%Z).

Axiom dist_concat_left : forall (u:(list char)) (v:(list char)) (w:(list
  char)) (n:Z), (dist v w n) -> (dist (infix_plpl u v) w ((length u) + n)%Z).

Axiom dist_concat_right : forall (u:(list char)) (v:(list char)) (w:(list
  char)) (n:Z), (dist v w n) -> (dist v (infix_plpl u w) ((length u) + n)%Z).

Axiom min_dist_equal : forall (w1:(list char)) (w2:(list char)) (a:char)
  (n:Z), (min_dist w1 w2 n) -> (min_dist (Cons a w1) (Cons a w2) n).

Axiom min_dist_diff : forall (w1:(list char)) (w2:(list char)) (a:char)
  (b:char) (m:Z) (p:Z), (~ (a = b)) -> ((min_dist (Cons a w1) w2 p) ->
  ((min_dist w1 (Cons b w2) m) -> (min_dist (Cons a w1) (Cons b w2)
  ((Zmin m p) + 1%Z)%Z))).

Axiom min_dist_eps : forall (w:(list char)) (a:char) (n:Z), (min_dist w
  (Nil :(list char)) n) -> (min_dist (Cons a w) (Nil :(list char))
  (n + 1%Z)%Z).

Axiom min_dist_eps_length : forall (w:(list char)), (min_dist (Nil :(list
  char)) w (length w)).

(* Why3 assumption *)
Inductive ref (a:Type) {a_WT:WhyType a} :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a] [a_WT]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a}(v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

Axiom map : forall (a:Type) {a_WT:WhyType a} (b:Type) {b_WT:WhyType b}, Type.
Parameter map_WhyType : forall (a:Type) {a_WT:WhyType a}
  (b:Type) {b_WT:WhyType b}, WhyType (map a b).
Existing Instance map_WhyType.

Parameter get: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b.

Parameter set: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  (map a b) -> a -> b -> (map a b).

Axiom Select_eq : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (m:(map a b)), forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) ->
  ((get (set m a1 b1) a2) = b1).

Axiom Select_neq : forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, forall (m:(map a b)), forall (a1:a) (a2:a),
  forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1) a2) = (get m a2)).

Parameter const: forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  b -> (map a b).

Axiom Const : forall {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b},
  forall (b1:b) (a1:a), ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Inductive array (a:Type) {a_WT:WhyType a} :=
  | mk_array : Z -> (map Z a) -> array a.
Axiom array_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (array a).
Existing Instance array_WhyType.
Implicit Arguments mk_array [[a] [a_WT]].

(* Why3 assumption *)
Definition elts {a:Type} {a_WT:WhyType a}(v:(array a)): (map Z a) :=
  match v with
  | (mk_array x x1) => x1
  end.

(* Why3 assumption *)
Definition length1 {a:Type} {a_WT:WhyType a}(v:(array a)): Z :=
  match v with
  | (mk_array x x1) => x
  end.

(* Why3 assumption *)
Definition get1 {a:Type} {a_WT:WhyType a}(a1:(array a)) (i:Z): a :=
  (get (elts a1) i).

(* Why3 assumption *)
Definition set1 {a:Type} {a_WT:WhyType a}(a1:(array a)) (i:Z) (v:a): (array
  a) := (mk_array (length1 a1) (set (elts a1) i v)).

(* Why3 assumption *)
Definition make {a:Type} {a_WT:WhyType a}(n:Z) (v:a): (array a) :=
  (mk_array n (const v:(map Z a))).

Parameter suffix: (array char) -> Z -> (list char).

Axiom suffix_nil : forall (a:(array char)), ((suffix a
  (length1 a)) = (Nil :(list char))).

Axiom suffix_cons : forall (a:(array char)) (i:Z), ((0%Z <= i)%Z /\
  (i < (length1 a))%Z) -> ((suffix a i) = (Cons (get1 a i) (suffix a
  (i + 1%Z)%Z))).

Axiom suffix_length : forall (a:(array char)) (i:Z), ((0%Z <= i)%Z /\
  (i <= (length1 a))%Z) -> ((length (suffix a i)) = ((length1 a) - i)%Z).

(* Why3 assumption *)
Definition min_suffix(a1:(array char)) (a2:(array char)) (i:Z) (j:Z)
  (n:Z): Prop := (min_dist (suffix a1 i) (suffix a2 j) n).

Require Import Why3.
Ltac ae := why3 "alt-ergo" timelimit 3.
Open Scope Z_scope.

(* Why3 goal *)
Theorem WP_parameter_distance : forall (w1:Z) (w2:Z), forall (w21:(map Z
  char)) (w11:(map Z char)), let w22 := (mk_array w2 w21) in let w12 :=
  (mk_array w1 w11) in (((0%Z <= w1)%Z /\ (0%Z <= w2)%Z) ->
  ((0%Z <= (w2 + 1%Z)%Z)%Z -> ((0%Z <= w2)%Z -> forall (t:(map Z Z)),
  (forall (j:Z), ((0%Z <= j)%Z /\ (j < (w2 + 1%Z)%Z)%Z) -> ((get t
  j) = (w2 - j)%Z)) -> ((0%Z <= (w1 - 1%Z)%Z)%Z -> forall (t1:(map Z Z)),
  forall (i:Z), ((i <= (w1 - 1%Z)%Z)%Z /\ (0%Z <= i)%Z) -> ((forall (j:Z),
  ((0%Z <= j)%Z /\ (j <= w2)%Z) -> (min_dist (suffix w12 (i + 1%Z)%Z)
  (suffix w22 j) (get t1 j))) -> (((0%Z <= w2)%Z /\ (w2 < (w2 + 1%Z)%Z)%Z) ->
  (((0%Z <= w2)%Z /\ (w2 < (w2 + 1%Z)%Z)%Z) -> (((0%Z <= w2)%Z /\
  (w2 < (w2 + 1%Z)%Z)%Z) -> forall (t2:(map Z Z)), (t2 = (set t1 w2 ((get t1
  w2) + 1%Z)%Z)) -> (((w2 - 1%Z)%Z < 0%Z)%Z -> forall (j:Z), ((0%Z <= j)%Z /\
  (j <= w2)%Z) -> (min_dist (suffix w12 ((i - 1%Z)%Z + 1%Z)%Z) (suffix w22 j)
  (get t2 j))))))))))).
intros w1 w2 w21 w11 w22 w12 (h1,h2) h3 h4 t h5 h6 t1 i (h7,h8) h9 (h10,h11)
(h12,h13) (h14,h15) t2 h16 h17 j (h18,h19).
replace (i-1+1) with i by omega.
rewrite suffix_cons.
assert (h : length1 w22 = j) by ae.
ae.
ae.
Qed.


