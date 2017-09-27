(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require list.List.
Require list.Length.
Require list.Mem.
Require map.Map.
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

Axiom map : forall (a:Type) (b:Type), Type.
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
  forall (b1:b) (a1:a), ((get (const b1: (map a b)) a1) = b1).

(* Why3 assumption *)
Inductive id :=
  | Id : Z -> id.
Axiom id_WhyType : WhyType id.
Existing Instance id_WhyType.

(* Why3 assumption *)
Definition state := (map id Z).

(* Why3 assumption *)
Definition pos := Z.

(* Why3 assumption *)
Definition stack := (list Z).

(* Why3 assumption *)
Inductive machine_state :=
  | VMS : Z -> (list Z) -> (map id Z) -> machine_state.
Axiom machine_state_WhyType : WhyType machine_state.
Existing Instance machine_state_WhyType.

(* Why3 assumption *)
Inductive instr :=
  | Iconst : Z -> instr
  | Ivar : id -> instr
  | Isetvar : id -> instr
  | Ibranch : Z -> instr
  | Iadd : instr
  | Isub : instr
  | Imul : instr
  | Ibeq : Z -> instr
  | Ibne : Z -> instr
  | Ible : Z -> instr
  | Ibgt : Z -> instr
  | Ihalt : instr.
Axiom instr_WhyType : WhyType instr.
Existing Instance instr_WhyType.

(* Why3 assumption *)
Definition code := (list instr).

(* Why3 assumption *)
Inductive codeseq_at: (list instr) -> Z -> (list instr) -> Prop :=
  | codeseq_at_intro : forall (c1:(list instr)) (c2:(list instr))
      (c3:(list instr)), (codeseq_at
      (Init.Datatypes.app (Init.Datatypes.app c1 c2) c3)
      (list.Length.length c1) c2).

Axiom codeseq_at_app_right : forall (c:(list instr)) (c1:(list instr))
  (c2:(list instr)) (p:Z), (codeseq_at c p (Init.Datatypes.app c1 c2)) ->
  (codeseq_at c (p + (list.Length.length c1))%Z c2).

Axiom codeseq_at_app_left : forall (c:(list instr)) (c1:(list instr))
  (c2:(list instr)) (p:Z), (codeseq_at c p (Init.Datatypes.app c1 c2)) ->
  (codeseq_at c p c1).

(* Why3 assumption *)
Definition iconst (n:Z): (list instr) :=
  (Init.Datatypes.cons (Iconst n) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ivar (x:id): (list instr) :=
  (Init.Datatypes.cons (Ivar x) Init.Datatypes.nil).

(* Why3 assumption *)
Definition isetvar (x:id): (list instr) :=
  (Init.Datatypes.cons (Isetvar x) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibeq (ofs:Z): (list instr) :=
  (Init.Datatypes.cons (Ibeq ofs) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ible (ofs:Z): (list instr) :=
  (Init.Datatypes.cons (Ible ofs) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibne (ofs:Z): (list instr) :=
  (Init.Datatypes.cons (Ibne ofs) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibgt (ofs:Z): (list instr) :=
  (Init.Datatypes.cons (Ibgt ofs) Init.Datatypes.nil).

(* Why3 assumption *)
Definition ibranch (ofs:Z): (list instr) :=
  (Init.Datatypes.cons (Ibranch ofs) Init.Datatypes.nil).

(* Why3 assumption *)
Inductive transition: (list instr) -> machine_state -> machine_state ->
  Prop :=
  | trans_const : forall (c:(list instr)) (p:Z) (n:Z), (codeseq_at c p
      (iconst n)) -> forall (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      s m) (VMS (p + 1%Z)%Z (Init.Datatypes.cons n s) m))
  | trans_var : forall (c:(list instr)) (p:Z) (x:id), (codeseq_at c p
      (ivar x)) -> forall (s:(list Z)) (m:(map id Z)), (transition c (VMS p s
      m) (VMS (p + 1%Z)%Z (Init.Datatypes.cons (get m x) s) m))
  | trans_set_var : forall (c:(list instr)) (p:Z) (x:id), (codeseq_at c p
      (isetvar x)) -> forall (n:Z) (s:(list Z)) (m:(map id Z)), (transition c
      (VMS p (Init.Datatypes.cons n s) m) (VMS (p + 1%Z)%Z s (set m x n)))
  | trans_add : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Iadd Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 + n2)%Z s) m))
  | trans_sub : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Isub Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 - n2)%Z s) m))
  | trans_mul : forall (c:(list instr)) (p:Z), (codeseq_at c p
      (Init.Datatypes.cons Imul Init.Datatypes.nil)) -> forall (n1:Z) (n2:Z)
      (s:(list Z)) (m:(map id Z)), (transition c (VMS p
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m) (VMS (p + 1%Z)%Z
      (Init.Datatypes.cons (n1 * n2)%Z s) m))
  | trans_beq : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibeq ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 = n2) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs)%Z s m))
  | trans_beq1 : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibeq ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 = n2)) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bne : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibne ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 = n2) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bne1 : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibne ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 = n2)) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs)%Z s m))
  | trans_ble : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ible ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 <= n2)%Z -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs)%Z s m))
  | trans_ble1 : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ible ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 <= n2)%Z) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bgt : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibgt ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (n1 <= n2)%Z -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS (p1 + 1%Z)%Z s m))
  | trans_bgt1 : forall (c:(list instr)) (p1:Z) (ofs:Z), (codeseq_at c p1
      (ibgt ofs)) -> forall (s:(list Z)) (m:(map id Z)) (n1:Z) (n2:Z),
      (~ (n1 <= n2)%Z) -> (transition c (VMS p1
      (Init.Datatypes.cons n2 (Init.Datatypes.cons n1 s)) m)
      (VMS ((p1 + 1%Z)%Z + ofs)%Z s m))
  | trans_branch : forall (c:(list instr)) (p:Z) (ofs:Z), (codeseq_at c p
      (ibranch ofs)) -> forall (s:(list Z)) (m:(map id Z)), (transition c
      (VMS p s m) (VMS ((p + 1%Z)%Z + ofs)%Z s m)).

(* Why3 assumption *)
Inductive transition_star: (list instr) -> machine_state -> machine_state ->
  Prop :=
  | Refl : forall (p:(list instr)) (x:machine_state), (transition_star p x x)
  | Step : forall (p:(list instr)) (x:machine_state) (y:machine_state)
      (z:machine_state), (transition p x y) -> ((transition_star p y z) ->
      (transition_star p x z)).

Axiom transition_star_one : forall (p:(list instr)) (s1:machine_state)
  (s2:machine_state), (transition p s1 s2) -> (transition_star p s1 s2).

Axiom transition_star_transitive : forall (p:(list instr)) (s1:machine_state)
  (s2:machine_state) (s3:machine_state), (transition_star p s1 s2) ->
  ((transition_star p s2 s3) -> (transition_star p s1 s3)).

(* Why3 assumption *)
Definition vm_terminates (c:(list instr)) (mi:(map id Z)) (mf:(map id
  Z)): Prop := exists p:Z, (codeseq_at c p
  (Init.Datatypes.cons Ihalt Init.Datatypes.nil)) /\ (transition_star c
  (VMS 0%Z Init.Datatypes.nil mi) (VMS p Init.Datatypes.nil mf)).

Axiom func : forall (a:Type) (b:Type), Type.
Parameter func_WhyType : forall (a:Type) {a_WT:WhyType a}
  (b:Type) {b_WT:WhyType b}, WhyType (func a b).
Existing Instance func_WhyType.

(* Why3 assumption *)
Definition pred (a:Type) := (func a bool).

Parameter infix_at: forall {a:Type} {a_WT:WhyType a}
  {b:Type} {b_WT:WhyType b}, (func a b) -> a -> b.

(* Why3 assumption *)
Inductive clock_state :=
  | VMC : Z -> (list Z) -> (map id Z) -> Z -> clock_state.
Axiom clock_state_WhyType : WhyType clock_state.
Existing Instance clock_state_WhyType.

(* Why3 assumption *)
Definition parameter := ((list instr)* (func clock_state bool))%type.

(* Why3 assumption *)
Definition transition_out (p:((list instr)* (func clock_state bool))%type)
  (mc1:clock_state) (mc2:clock_state): Prop :=
  match p with
  | (c, inside) => (~ ((infix_at inside mc1) = true)) /\
      ((~ ((infix_at inside mc2) = true)) /\
      match mc1 with
      | (VMC p1 s1 m1 c1) =>
          match mc2 with
          | (VMC p2 s2 m2 c2) => (transition c (VMS p1 s1 m1) (VMS p2 s2
              m2)) /\ (c2 = (c1 + 1%Z)%Z)
          end
      end)
  end.

(* Why3 assumption *)
Inductive transition_star1: ((list instr)* (func clock_state bool))%type ->
  clock_state -> clock_state -> Prop :=
  | Refl1 : forall (p:((list instr)* (func clock_state bool))%type)
      (x:clock_state), (transition_star1 p x x)
  | Step1 : forall (p:((list instr)* (func clock_state bool))%type)
      (x:clock_state) (y:clock_state) (z:clock_state), (transition_out p x
      y) -> ((transition_star1 p y z) -> (transition_star1 p x z)).

Axiom transition_star_one1 : forall (p:((list instr)* (func clock_state
  bool))%type) (s1:clock_state) (s2:clock_state), (transition_out p s1 s2) ->
  (transition_star1 p s1 s2).

Axiom transition_star_transitive1 : forall (p:((list instr)* (func
  clock_state bool))%type) (s1:clock_state) (s2:clock_state)
  (s3:clock_state), (transition_star1 p s1 s2) -> ((transition_star1 p s2
  s3) -> (transition_star1 p s1 s3)).

Axiom transition_star_endpoints : forall (c:(list instr)) (p:(func
  clock_state bool)) (s0:clock_state) (s1:clock_state), (transition_star1 (c,
  p) s0 s1) -> ((~ (s0 = s1)) -> ((~ ((infix_at p s0) = true)) /\
  ~ ((infix_at p s1) = true))).

Axiom transition_star_weakening : forall (c:(list instr)) (p:(func
  clock_state bool)) (q:(func clock_state bool)) (s0:clock_state)
  (s1:clock_state), (forall (x:clock_state), ((infix_at q x) = true) ->
  ((infix_at p x) = true)) -> ((transition_star1 (c, p) s0 s1) ->
  (transition_star1 (c, q) s0 s1)).

Axiom transition_deterministic : forall (c:(list instr)) (ms:machine_state)
  (ms':machine_state) (ms'':machine_state), ((transition c ms ms') /\
  (transition c ms ms'')) -> (ms' = ms'').

Axiom transition_out_deterministic : forall (p:((list instr)* (func
  clock_state bool))%type) (mc1:clock_state) (mc2:clock_state)
  (mc3:clock_state), ((transition_out p mc1 mc2) /\ (transition_out p mc1
  mc3)) -> (mc2 = mc3).

(* Why3 assumption *)
Definition fst {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} (p:(a*
  b)%type): a := match p with
  | (x, _) => x
  end.

(* Why3 assumption *)
Definition snd {a:Type} {a_WT:WhyType a} {b:Type} {b_WT:WhyType b} (p:(a*
  b)%type): b := match p with
  | (_, y) => y
  end.

(* Why3 assumption *)
Definition pred1 := (func clock_state bool).

(* Why3 assumption *)
Definition rel := (func clock_state (func clock_state bool)).

(* Why3 assumption *)
Definition pre (a:Type) := (func a (func Z (func clock_state bool))).

(* Why3 assumption *)
Definition post (a:Type) := (func a (func Z (func clock_state (func
  clock_state bool)))).

(* Why3 assumption *)
Inductive hl
  (a:Type) :=
  | mk_hl : (list instr) -> (func a (func Z (func clock_state bool))) ->
      (func a (func Z (func clock_state (func clock_state bool)))) -> hl a.
Axiom hl_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (hl a).
Existing Instance hl_WhyType.
Implicit Arguments mk_hl [[a]].

(* Why3 assumption *)
Definition post1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (func a (func Z (func
  clock_state (func clock_state bool)))) :=
  match v with
  | (mk_hl x x1 x2) => x2
  end.

(* Why3 assumption *)
Definition pre1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (func a (func Z (func
  clock_state bool))) := match v with
  | (mk_hl x x1 x2) => x1
  end.

(* Why3 assumption *)
Definition code1 {a:Type} {a_WT:WhyType a} (v:(hl a)): (list instr) :=
  match v with
  | (mk_hl x x1 x2) => x
  end.

(* Why3 assumption *)
Definition wp_trans (a:Type) := (func a (func Z (func (func clock_state bool)
  (func clock_state bool)))).

(* Why3 assumption *)
Inductive wp
  (a:Type) :=
  | mk_wp : (list instr) -> (func a (func Z (func (func clock_state bool)
      (func clock_state bool)))) -> wp a.
Axiom wp_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (wp a).
Existing Instance wp_WhyType.
Implicit Arguments mk_wp [[a]].

(* Why3 assumption *)
Definition wp1 {a:Type} {a_WT:WhyType a} (v:(wp a)): (func a (func Z (func
  (func clock_state bool) (func clock_state bool)))) :=
  match v with
  | (mk_wp x x1) => x1
  end.

(* Why3 assumption *)
Definition wcode {a:Type} {a_WT:WhyType a} (v:(wp a)): (list instr) :=
  match v with
  | (mk_wp x x1) => x
  end.

(* Why3 assumption *)
Definition hl_correctness {a:Type} {a_WT:WhyType a} (cs:(hl a)): Prop :=
  forall (x:a) (c_glob:(list instr)) (p:Z) (mc:clock_state)
  (mc':clock_state), ((infix_at (infix_at (infix_at (pre1 cs) x) p)
  mc) = true) -> ((codeseq_at c_glob p (code1 cs)) -> let post2 :=
  (infix_at (infix_at (infix_at (post1 cs) x) p) mc) in (((transition_star1 (
  c_glob, post2) mc mc') /\ ~ ((infix_at post2 mc') = true)) ->
  match mc' with
  | (VMC p1 s m _) => exists ms'':machine_state, (transition c_glob (VMS p1 s
      m) ms'')
  end)).

(* Why3 assumption *)
Definition wp_correctness {a:Type} {a_WT:WhyType a} (code2:(wp a)): Prop :=
  forall (x:a) (c_glob:(list instr)) (p:Z) (post2:(func clock_state bool))
  (mc:clock_state) (mc':clock_state),
  ((infix_at (infix_at (infix_at (infix_at (wp1 code2) x) p) post2)
  mc) = true) -> ((codeseq_at c_glob p (wcode code2)) -> (((transition_star1
  (c_glob, post2) mc mc') /\ ~ ((infix_at post2 mc') = true)) ->
  match mc' with
  | (VMC p1 s m _) => exists ms'':machine_state, (transition c_glob (VMS p1 s
      m) ms'')
  end)).

Parameter seq_wp: forall {a:Type} {a_WT:WhyType a}, Z -> (func a (func Z
  (func (func clock_state bool) (func clock_state bool)))) -> (func (a*
  clock_state)%type (func Z (func (func clock_state bool) (func clock_state
  bool)))) -> (func a (func Z (func (func clock_state bool) (func clock_state
  bool)))).

Axiom seq_wp_def : forall {a:Type} {a_WT:WhyType a}, forall (l1:Z) (w1:(func
  a (func Z (func (func clock_state bool) (func clock_state bool)))))
  (w2:(func (a* clock_state)%type (func Z (func (func clock_state bool) (func
  clock_state bool))))) (x:a) (p:Z) (q:(func clock_state bool))
  (mc:clock_state), ((infix_at (infix_at (infix_at (infix_at (seq_wp l1 w1
  w2) x) p) q) mc) = (infix_at (infix_at (infix_at (infix_at w1 x) p)
  (infix_at (infix_at (infix_at w2 (x, mc)) (p + l1)%Z) q)) mc)).

Axiom seq_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (l1:Z)
  (w1:(func a (func Z (func (func clock_state bool) (func clock_state
  bool))))) (w2:(func (a* clock_state)%type (func Z (func (func clock_state
  bool) (func clock_state bool))))) (x:a) (p:Z) (q:(func clock_state bool))
  (mc:clock_state), ((infix_at (infix_at (infix_at (infix_at (seq_wp l1 w1
  w2) x) p) q) mc) = (infix_at (infix_at (infix_at (infix_at w1 x) p)
  (infix_at (infix_at (infix_at w2 (x, mc)) (p + l1)%Z) q)) mc)).

Parameter fork_wp: forall {a:Type} {a_WT:WhyType a}, (func a (func Z (func
  (func clock_state bool) (func clock_state bool)))) -> (func a (func Z (func
  clock_state bool))) -> (func a (func Z (func (func clock_state bool) (func
  clock_state bool)))).

Axiom fork_wp_def : forall {a:Type} {a_WT:WhyType a}, forall (w:(func a (func
  Z (func (func clock_state bool) (func clock_state bool))))) (cond:(func a
  (func Z (func clock_state bool)))) (x:a) (p:Z) (q:(func clock_state bool))
  (ms:clock_state), ((infix_at (infix_at (infix_at (infix_at (fork_wp w cond)
  x) p) q) ms) = true) <-> (((~ ((infix_at (infix_at (infix_at cond x) p)
  ms) = true)) -> ((infix_at q ms) = true)) /\
  (((infix_at (infix_at (infix_at cond x) p) ms) = true) ->
  ((infix_at (infix_at (infix_at (infix_at w x) p) q) ms) = true))).

Axiom fork_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (w:(func a
  (func Z (func (func clock_state bool) (func clock_state bool)))))
  (cond:(func a (func Z (func clock_state bool)))) (x:a) (p:Z) (q:(func
  clock_state bool)) (ms:clock_state),
  ((infix_at (infix_at (infix_at (infix_at (fork_wp w cond) x) p) q)
  ms) = true) <-> (((~ ((infix_at (infix_at (infix_at cond x) p)
  ms) = true)) -> ((infix_at q ms) = true)) /\
  (((infix_at (infix_at (infix_at cond x) p) ms) = true) ->
  ((infix_at (infix_at (infix_at (infix_at w x) p) q) ms) = true))).

Parameter towp_wp: forall {a:Type} {a_WT:WhyType a}, (func a (func Z (func
  clock_state bool))) -> (func a (func Z (func clock_state (func clock_state
  bool)))) -> (func a (func Z (func (func clock_state bool) (func clock_state
  bool)))).

Axiom towp_wp_def : forall {a:Type} {a_WT:WhyType a}, forall (pr:(func a
  (func Z (func clock_state bool)))) (ps:(func a (func Z (func clock_state
  (func clock_state bool))))) (x:a) (p:Z) (q:(func clock_state bool))
  (ms:clock_state), ((infix_at (infix_at (infix_at (infix_at (towp_wp pr ps)
  x) p) q) ms) = true) <-> (((infix_at (infix_at (infix_at pr x) p)
  ms) = true) /\ forall (ms':clock_state),
  ((infix_at (infix_at (infix_at (infix_at ps x) p) ms) ms') = true) ->
  ((infix_at q ms') = true)).

Axiom towp_wp_lemma : forall {a:Type} {a_WT:WhyType a}, forall (pr:(func a
  (func Z (func clock_state bool)))) (ps:(func a (func Z (func clock_state
  (func clock_state bool))))) (x:a) (p:Z) (q:(func clock_state bool))
  (ms:clock_state), ((infix_at (infix_at (infix_at (infix_at (towp_wp pr ps)
  x) p) q) ms) = true) <-> (((infix_at (infix_at (infix_at pr x) p)
  ms) = true) /\ forall (ms':clock_state),
  ((infix_at (infix_at (infix_at (infix_at ps x) p) ms) ms') = true) ->
  ((infix_at q ms') = true)).

Parameter trivial_pre: forall {a:Type} {a_WT:WhyType a}, (func a (func Z
  (func clock_state bool))).

Axiom trivial_pre_def : forall {a:Type} {a_WT:WhyType a}, forall (x:a) (p:Z)
  (ms:clock_state), ((infix_at (infix_at (infix_at (trivial_pre : (func a
  (func Z (func clock_state bool)))) x) p) ms) = true) <->
  match ms with
  | (VMC p' _ _ _) => (p = p')
  end.

Parameter loop_preservation: forall {a:Type} {a_WT:WhyType a}, (func a (func
  Z (func clock_state bool))) -> (func a (func Z (func clock_state bool))) ->
  (func a (func Z (func clock_state (func clock_state bool)))).

Axiom loop_preservation_def : forall {a:Type} {a_WT:WhyType a},
  forall (inv:(func a (func Z (func clock_state bool)))) (post2:(func a (func
  Z (func clock_state bool)))) (x:a) (p:Z) (ms:clock_state)
  (ms':clock_state),
  ((infix_at (infix_at (infix_at (infix_at (loop_preservation inv post2) x)
  p) ms) ms') = true) <-> ((((infix_at (infix_at (infix_at inv x) p)
  ms') = true) /\ ~ (ms = ms')) \/ ((infix_at (infix_at (infix_at post2 x) p)
  ms') = true)).

Parameter forget_old: forall {a:Type} {a_WT:WhyType a}, (func a (func Z (func
  clock_state bool))) -> (func a (func Z (func clock_state (func clock_state
  bool)))).

Axiom forget_old_def : forall {a:Type} {a_WT:WhyType a}, forall (post2:(func
  a (func Z (func clock_state bool)))) (x:a) (p:Z) (ms:clock_state),
  ((infix_at (infix_at (infix_at (forget_old post2) x) p)
  ms) = (infix_at (infix_at post2 x) p)).

Parameter ifun_post: forall {a:Type} {a_WT:WhyType a}, (func machine_state
  machine_state) -> (func a (func Z (func clock_state (func clock_state
  bool)))).

Axiom ifun_post_def : forall {a:Type} {a_WT:WhyType a}, forall (f:(func
  machine_state machine_state)) (x:a) (p:Z) (mc:clock_state)
  (mc':clock_state),
  ((infix_at (infix_at (infix_at (infix_at (ifun_post f: (func a (func Z
  (func clock_state (func clock_state bool))))) x) p) mc) mc') = true) <->
  match mc with
  | (VMC p1 s m c) =>
      match mc' with
      | (VMC p' s' m' c') => ((VMS p' s' m') = (infix_at f (VMS p1 s m))) /\
          (c' = (c + 1%Z)%Z)
      end
  end.

(* Why3 goal *)
Theorem WP_parameter_ifunf : forall {a:Type} {a_WT:WhyType a},
  forall (pre2:(func a (func Z (func clock_state bool))))
  (code_f:(list instr)) (f:(func machine_state machine_state)),
  (forall (c:(list instr)) (p:Z), (codeseq_at c p code_f) -> forall (x:a)
  (ms:machine_state) (clock:Z),
  match ms with
  | (VMS p' s m) => ((infix_at (infix_at (infix_at pre2 x) p) (VMC p' s m
      clock)) = true) -> (transition c ms (infix_at f ms))
  end) -> forall (x:a) (c_glob:(list instr)) (p:Z) (mc:clock_state)
  (mc':clock_state), ((infix_at (infix_at (infix_at pre2 x) p) mc) = true) ->
  ((codeseq_at c_glob p code_f) -> let post2 :=
  (infix_at (infix_at (infix_at (ifun_post f: (func a (func Z (func
  clock_state (func clock_state bool))))) x) p) mc) in (((transition_star1 (
  c_glob, post2) mc mc') /\ ~ ((infix_at post2 mc') = true)) -> forall (x1:Z)
  (x2:(list Z)) (x3:(map id Z)) (x4:Z), (mc' = (VMC x1 x2 x3 x4)) ->
  (mc = mc'))).
intros a a_WT pre2 code_f f h1 x c_glob p mc mc' h2 h3 post2 (h4,h5) x1 x2 x3
x4 h6.
remember post2 as post;unfold post2 in *;clear post2.
inversion h4;subst. trivial.
Require Import Why3.
Ltac cvc := why3 "CVC4,1.4,".
assert (infix_at (infix_at (infix_at (infix_at (ifun_post f) x) p) mc) y = true).
destruct y. destruct mc.
assert (infix_at f (VMS z1 l0 m0) = VMS z l m).
specialize (h1 c_glob p h3 x (VMS z1 l0 m0)). simpl in h1.
eapply transition_deterministic. split. eapply h1. eassumption. 
cvc. rewrite ifun_post_def;cvc. cvc.
Qed.

