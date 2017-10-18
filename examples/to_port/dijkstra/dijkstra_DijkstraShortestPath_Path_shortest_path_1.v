(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require HighOrd.
Require int.Int.
Require map.Map.
Require map.Const.

(* Why3 assumption *)
Inductive ref (a:Type) :=
  | mk_ref : a -> ref a.
Axiom ref_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (ref a).
Existing Instance ref_WhyType.
Implicit Arguments mk_ref [[a]].

(* Why3 assumption *)
Definition contents {a:Type} {a_WT:WhyType a} (v:(ref a)): a :=
  match v with
  | (mk_ref x) => x
  end.

Axiom set : forall (a:Type), Type.
Parameter set_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (set a).
Existing Instance set_WhyType.

Parameter mem: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> Prop.

Parameter infix_eqeq: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) ->
  Prop.

Axiom infix_eqeq_spec : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (infix_eqeq s1 s2) <-> forall (x:a), (mem x s1) <-> (mem x
  s2).

Axiom extensionality : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (infix_eqeq s1 s2) -> (s1 = s2).

Parameter subset: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) ->
  Prop.

Axiom subset_spec : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset s1 s2) <-> forall (x:a), (mem x s1) -> (mem x s2).

Axiom subset_refl : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (subset s s).

Axiom subset_trans : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)) (s3:(set a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1
  s3)).

Parameter is_empty: forall {a:Type} {a_WT:WhyType a}, (set a) -> Prop.

Axiom is_empty_spec : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (is_empty s) <-> forall (x:a), ~ (mem x s).

Parameter empty: forall {a:Type} {a_WT:WhyType a}, (set a).

Axiom empty_def : forall {a:Type} {a_WT:WhyType a}, (is_empty (empty : (set
  a))).

Parameter add: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> (set a).

Axiom add_spec : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set a)),
  forall (y:a), (mem y (add x s)) <-> ((y = x) \/ (mem y s)).

Parameter remove: forall {a:Type} {a_WT:WhyType a}, a -> (set a) -> (set a).

Axiom remove_spec : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set
  a)), forall (y:a), (mem y (remove x s)) <-> ((~ (y = x)) /\ (mem y s)).

Axiom add_remove : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set
  a)), (mem x s) -> ((add x (remove x s)) = s).

Axiom remove_add : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set
  a)), ((remove x (add x s)) = (remove x s)).

Axiom subset_remove : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (s:(set
  a)), (subset (remove x s) s).

Parameter union: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom union_spec : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), forall (x:a), (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x
  s2)).

Parameter inter: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom inter_spec : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), forall (x:a), (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x
  s2)).

Parameter diff: forall {a:Type} {a_WT:WhyType a}, (set a) -> (set a) -> (set
  a).

Axiom diff_spec : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), forall (x:a), (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem
  x s2)).

Axiom subset_diff : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset (diff s1 s2) s1).

Parameter choose: forall {a:Type} {a_WT:WhyType a}, (set a) -> a.

Axiom choose_spec : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (~ (is_empty s)) -> (mem (choose s) s).

Parameter cardinal: forall {a:Type} {a_WT:WhyType a}, (set a) -> Z.

Axiom cardinal_nonneg : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  (0%Z <= (cardinal s))%Z.

Axiom cardinal_empty : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  ((cardinal s) = 0%Z) <-> (is_empty s).

Axiom cardinal_add : forall {a:Type} {a_WT:WhyType a}, forall (x:a),
  forall (s:(set a)), (~ (mem x s)) -> ((cardinal (add x
  s)) = (1%Z + (cardinal s))%Z).

Axiom cardinal_remove : forall {a:Type} {a_WT:WhyType a}, forall (x:a),
  forall (s:(set a)), (mem x s) -> ((cardinal s) = (1%Z + (cardinal (remove x
  s)))%Z).

Axiom cardinal_subset : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset s1 s2) -> ((cardinal s1) <= (cardinal s2))%Z.

Axiom subset_eq : forall {a:Type} {a_WT:WhyType a}, forall (s1:(set a))
  (s2:(set a)), (subset s1 s2) -> (((cardinal s1) = (cardinal s2)) ->
  (infix_eqeq s1 s2)).

Axiom cardinal1 : forall {a:Type} {a_WT:WhyType a}, forall (s:(set a)),
  ((cardinal s) = 1%Z) -> forall (x:a), (mem x s) -> (x = (choose s)).

Axiom vertex : Type.
Parameter vertex_WhyType : WhyType vertex.
Existing Instance vertex_WhyType.

Axiom t : Type.
Parameter t_WhyType : WhyType t.
Existing Instance t_WhyType.

Parameter contents1: t -> (set vertex).

Parameter is_empty1: t -> Prop.

Axiom is_empty_spec1 : forall (s:t), (is_empty1 s) <-> (is_empty
  (contents1 s)).

Parameter mem1: vertex -> t -> Prop.

Axiom mem_spec : forall (x:vertex) (s:t), (mem1 x s) <-> (mem x
  (contents1 s)).

Parameter cardinal2: t -> Z.

Axiom cardinal_spec : forall (s:t),
  ((cardinal2 s) = (cardinal (contents1 s))).

Axiom t1 : forall (a:Type), Type.
Parameter t1_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (t1 a).
Existing Instance t1_WhyType.

Parameter contents2: forall {a:Type} {a_WT:WhyType a}, (t1 a) -> (vertex ->
  a).

Parameter create: forall {a:Type} {a_WT:WhyType a}, a -> (t1 a).

Axiom create_spec : forall {a:Type} {a_WT:WhyType a}, forall (x:a),
  ((contents2 (create x)) = (map.Const.const x: (vertex -> a))).

Parameter mixfix_lbrb: forall {a:Type} {a_WT:WhyType a}, (t1 a) -> vertex ->
  a.

Axiom mixfix_lbrb_spec : forall {a:Type} {a_WT:WhyType a}, forall (m:(t1 a))
  (k:vertex), ((mixfix_lbrb m k) = ((contents2 m) k)).

Parameter mixfix_lblsmnrb: forall {a:Type} {a_WT:WhyType a}, (t1 a) ->
  vertex -> a -> (t1 a).

Axiom mixfix_lblsmnrb_spec : forall {a:Type} {a_WT:WhyType a}, forall (m:(t1
  a)) (k:vertex) (v:a), ((contents2 (mixfix_lblsmnrb m k
  v)) = (map.Map.set (contents2 m) k v)).

Parameter v: (set vertex).

Parameter g_succ: vertex -> (set vertex).

Axiom g_succ_spec : forall (x:vertex), (subset (g_succ x) v).

Parameter weight: vertex -> vertex -> Z.

Axiom weight_spec : forall (us:vertex) (us1:vertex), (0%Z <= (weight us
  us1))%Z.

(* Why3 assumption *)
Definition min (m:vertex) (q:t) (d:(t1 Z)): Prop := (mem1 m q) /\
  forall (x:vertex), (mem1 x q) -> ((mixfix_lbrb d m) <= (mixfix_lbrb d
  x))%Z.

(* Why3 assumption *)
Inductive path: vertex -> vertex -> Z -> Prop :=
  | Path_nil : forall (x:vertex), (path x x 0%Z)
  | Path_cons : forall (x:vertex) (y:vertex) (z:vertex), forall (d:Z), (path
      x y d) -> ((mem z (g_succ y)) -> (path x z (d + (weight y z))%Z)).

Axiom Length_nonneg : forall (x:vertex) (y:vertex), forall (d:Z), (path x y
  d) -> (0%Z <= d)%Z.

(* Why3 assumption *)
Definition shortest_path (x:vertex) (y:vertex) (d:Z): Prop := (path x y d) /\
  forall (d':Z), (path x y d') -> (d <= d')%Z.

Axiom Path_inversion : forall (src:vertex) (v1:vertex), forall (d:Z), (path
  src v1 d) -> (((v1 = src) /\ (d = 0%Z)) \/ exists v':vertex, (path src v'
  (d - (weight v' v1))%Z) /\ (mem v1 (g_succ v'))).

Require Import Why3. 
Ltac ae := why3 "Alt-Ergo,0.99.1," timelimit 5; admit.
Require Import Classical.

(* Why3 goal *)
Theorem Path_shortest_path : forall (src:vertex) (v1:vertex), forall (d:Z),
  (path src v1 d) -> exists d':Z, (shortest_path src v1 d') /\ (d' <= d)%Z.
(* Why3 intros src v1 d h1. *)
intros src v1 d path.
assert (0 <= d)%Z by ae.
generalize path; clear path. generalize H.
pattern d. apply Z_lt_induction. 2: trivial.
clear d H; intros d IH hd hpath.
destruct (classic (exists d', (d' < d)%Z /\ path src v1 d')).
destruct H as (d', (h1, h2)).
generalize (IH d'); ae.
exists d; ae.
Admitted.

