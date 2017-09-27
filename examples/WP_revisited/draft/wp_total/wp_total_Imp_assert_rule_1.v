(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Inductive datatype  :=
  | Tint : datatype 
  | Tbool : datatype .

Inductive operator  :=
  | Oplus : operator 
  | Ominus : operator 
  | Omult : operator 
  | Ole : operator .

Definition ident  := Z.

Inductive term  :=
  | Tconst : Z -> term 
  | Tvar : Z -> term 
  | Tderef : Z -> term 
  | Tbin : term -> operator -> term -> term .

Inductive fmla  :=
  | Fterm : term -> fmla 
  | Fand : fmla -> fmla -> fmla 
  | Fnot : fmla -> fmla 
  | Fimplies : fmla -> fmla -> fmla .

Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

Inductive value  :=
  | Vint : Z -> value 
  | Vbool : bool -> value .

Parameter map : forall (a:Type) (b:Type), Type.

Parameter get: forall (a:Type) (b:Type), (map a b) -> a -> b.

Implicit Arguments get.

Parameter set: forall (a:Type) (b:Type), (map a b) -> a -> b -> (map a b).

Implicit Arguments set.

Axiom Select_eq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (a1 = a2) -> ((get (set m a1 b1)
  a2) = b1).

Axiom Select_neq : forall (a:Type) (b:Type), forall (m:(map a b)),
  forall (a1:a) (a2:a), forall (b1:b), (~ (a1 = a2)) -> ((get (set m a1 b1)
  a2) = (get m a2)).

Parameter const: forall (b:Type) (a:Type), b -> (map a b).

Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (b:Type) (a:Type), forall (b1:b) (a1:a), ((get (const(
  b1):(map a b)) a1) = b1).

Definition env  := (map Z value).

Definition var_env  := (map Z value).

Definition ref_env  := (map Z value).

Inductive state  :=
  | mk_state : (map Z value) -> (map Z value) -> state .

Definition ref_env1(u:state): (map Z value) :=
  match u with
  | (mk_state _ ref_env2) => ref_env2
  end.

Definition var_env1(u:state): (map Z value) :=
  match u with
  | (mk_state var_env2 _) => var_env2
  end.

Parameter eval_bin: value -> operator -> value -> value.


Axiom eval_bin_def : forall (x:value) (op:operator) (y:value), match (x,
  y) with
  | ((Vint x1), (Vint y1)) =>
      match op with
      | Oplus => ((eval_bin x op y) = (Vint (x1 + y1)%Z))
      | Ominus => ((eval_bin x op y) = (Vint (x1 - y1)%Z))
      | Omult => ((eval_bin x op y) = (Vint (x1 * y1)%Z))
      | Ole => ((x1 <= y1)%Z -> ((eval_bin x op y) = (Vbool true))) /\
          ((~ (x1 <= y1)%Z) -> ((eval_bin x op y) = (Vbool false)))
      end
  | (_, _) => ((eval_bin x op y) = (Vbool false))
  end.

Set Implicit Arguments.
Fixpoint eval_term(s:state) (t:term) {struct t}: value :=
  match t with
  | (Tconst n) => (Vint n)
  | (Tvar id) => (get (var_env1 s) id)
  | (Tderef id) => (get (ref_env1 s) id)
  | (Tbin t1 op t2) => (eval_bin (eval_term s t1) op (eval_term s t2))
  end.
Unset Implicit Arguments.

Set Implicit Arguments.
Fixpoint eval_fmla(s:state) (f:fmla) {struct f}: Prop :=
  match f with
  | (Fterm t) => ((eval_term s t) = (Vbool true))
  | (Fand f1 f2) => (eval_fmla s f1) /\ (eval_fmla s f2)
  | (Fnot f1) => ~ (eval_fmla s f1)
  | (Fimplies f1 f2) => (eval_fmla s f1) -> (eval_fmla s f2)
  end.
Unset Implicit Arguments.

Parameter subst_term: term -> Z -> term -> term.


Axiom subst_term_def : forall (e:term) (x:Z) (t:term),
  match e with
  | (Tconst _) => ((subst_term e x t) = e)
  | (Tvar _) => ((subst_term e x t) = e)
  | (Tderef y) => ((x = y) -> ((subst_term e x t) = t)) /\ ((~ (x = y)) ->
      ((subst_term e x t) = e))
  | (Tbin e1 op e2) => ((subst_term e x t) = (Tbin (subst_term e1 x t) op
      (subst_term e2 x t)))
  end.

Axiom eval_subst_term : forall (s:state) (e:term) (x:Z) (t:term),
  ((eval_term s (subst_term e x t)) = (eval_term (mk_state (var_env1 s)
  (set (ref_env1 s) x (eval_term s t))) e)).

Set Implicit Arguments.
Fixpoint subst(f:fmla) (x:Z) (t:term) {struct f}: fmla :=
  match f with
  | (Fterm e) => (Fterm (subst_term e x t))
  | (Fand f1 f2) => (Fand (subst f1 x t) (subst f2 x t))
  | (Fnot f1) => (Fnot (subst f1 x t))
  | (Fimplies f1 f2) => (Fimplies (subst f1 x t) (subst f2 x t))
  end.
Unset Implicit Arguments.

Axiom eval_subst : forall (f:fmla) (s:state) (x:Z) (t:term), (eval_fmla s
  (subst f x t)) <-> (eval_fmla (mk_state (var_env1 s) (set (ref_env1 s) x
  (eval_term s t))) f).

Inductive stmt  :=
  | Sskip : stmt 
  | Sassign : Z -> term -> stmt 
  | Sseq : stmt -> stmt -> stmt 
  | Sif : term -> stmt -> stmt -> stmt 
  | Sassert : fmla -> stmt 
  | Swhile : term -> fmla -> stmt -> stmt .

Axiom check_skip : forall (s:stmt), (s = Sskip) \/ ~ (s = Sskip).

Inductive one_step : state -> stmt -> state -> stmt -> Prop :=
  | one_step_assign : forall (s:state) (x:Z) (e:term), (one_step s (Sassign x
      e) (mk_state (var_env1 s) (set (ref_env1 s) x (eval_term s e))) Sskip)
  | one_step_seq : forall (s:state) (sqt:state) (i1:stmt) (i1qt:stmt)
      (i2:stmt), (one_step s i1 sqt i1qt) -> (one_step s (Sseq i1 i2) sqt
      (Sseq i1qt i2))
  | one_step_seq_skip : forall (s:state) (i:stmt), (one_step s (Sseq Sskip i)
      s i)
  | one_step_if_true : forall (s:state) (e:term) (i1:stmt) (i2:stmt),
      ((eval_term s e) = (Vbool true)) -> (one_step s (Sif e i1 i2) s i1)
  | one_step_if_false : forall (s:state) (e:term) (i1:stmt) (i2:stmt),
      ((eval_term s e) = (Vbool false)) -> (one_step s (Sif e i1 i2) s i2)
  | one_step_assert : forall (s:state) (f:fmla), (eval_fmla s f) ->
      (one_step s (Sassert f) s Sskip)
  | one_step_while_true : forall (s:state) (e:term) (inv:fmla) (i:stmt),
      (eval_fmla s inv) -> (((eval_term s e) = (Vbool true)) -> (one_step s
      (Swhile e inv i) s (Sseq i (Swhile e inv i))))
  | one_step_while_false : forall (s:state) (e:term) (inv:fmla) (i:stmt),
      (eval_fmla s inv) -> (((eval_term s e) = (Vbool false)) -> (one_step s
      (Swhile e inv i) s Sskip)).

Inductive many_steps : state -> stmt -> state -> stmt -> Z -> Prop :=
  | many_steps_refl : forall (s:state) (i:stmt), (many_steps s i s i 0%Z)
  | many_steps_trans : forall (s1:state) (s2:state) (s3:state) (i1:stmt)
      (i2:stmt) (i3:stmt) (n:Z), (one_step s1 i1 s2 i2) -> ((many_steps s2 i2
      s3 i3 n) -> (many_steps s1 i1 s3 i3 (n + 1%Z)%Z)).

Axiom steps_non_neg : forall (s1:state) (s2:state) (i1:stmt) (i2:stmt) (n:Z),
  (many_steps s1 i1 s2 i2 n) -> (0%Z <= n)%Z.

Axiom many_steps_seq : forall (s1:state) (s3:state) (i1:stmt) (i2:stmt)
  (n:Z), (many_steps s1 (Sseq i1 i2) s3 Sskip n) -> exists s2:state,
  exists n1:Z, exists n2:Z, (many_steps s1 i1 s2 Sskip n1) /\ ((many_steps s2
  i2 s3 Sskip n2) /\ (n = ((1%Z + n1)%Z + n2)%Z)).

Definition valid_fmla(p:fmla): Prop := forall (s:state), (eval_fmla s p).

Definition valid_triple(p:fmla) (i:stmt) (q:fmla): Prop := forall (s:state),
  (eval_fmla s p) -> forall (sqt:state) (n:Z), (many_steps s i sqt Sskip
  n) -> (eval_fmla sqt q).

Axiom skip_rule : forall (q:fmla), (valid_triple q Sskip q).

Axiom assign_rule : forall (q:fmla) (x:Z) (e:term), (valid_triple (subst q x
  e) (Sassign x e) q).

Axiom seq_rule : forall (p:fmla) (q:fmla) (r:fmla) (i1:stmt) (i2:stmt),
  ((valid_triple p i1 r) /\ (valid_triple r i2 q)) -> (valid_triple p
  (Sseq i1 i2) q).

Axiom if_rule : forall (e:term) (p:fmla) (q:fmla) (i1:stmt) (i2:stmt),
  ((valid_triple (Fand p (Fterm e)) i1 q) /\ (valid_triple (Fand p
  (Fnot (Fterm e))) i2 q)) -> (valid_triple p (Sif e i1 i2) q).

(* YOU MAY EDIT THE CONTEXT BELOW *)

(* DO NOT EDIT BELOW *)

Theorem assert_rule : forall (f:fmla) (p:fmla), (valid_fmla (Fimplies p
  f)) -> (valid_triple p (Sassert f) p).
(* YOU MAY EDIT THE PROOF BELOW *)
intros f p H.
red.
intros s Hp s' n Hred.
inversion Hred; subst; clear Hred.
inversion H0; subst; clear H0.
inversion H1; subst; clear H1; auto.
inversion H0; subst; clear H0.
Qed.
(* DO NOT EDIT BELOW *)


