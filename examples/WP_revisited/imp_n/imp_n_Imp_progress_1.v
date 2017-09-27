(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require map.Map.
Require int.Int.

Axiom ident : Type.
Parameter ident_WhyType : WhyType ident.
Existing Instance ident_WhyType.

Axiom ident_eq_dec : forall (i1:ident) (i2:ident), (i1 = i2) \/ ~ (i1 = i2).

Parameter mk_ident: Z -> ident.

Axiom mk_ident_inj : forall (i:Z) (j:Z), ((mk_ident i) = (mk_ident j)) ->
  (i = j).

(* Why3 assumption *)
Inductive operator  :=
  | Oplus : operator 
  | Ominus : operator 
  | Omult : operator .
Axiom operator_WhyType : WhyType operator.
Existing Instance operator_WhyType.

(* Why3 assumption *)
Inductive expr  :=
  | Econst : Z -> expr 
  | Evar : ident -> expr 
  | Ebin : expr -> operator -> expr -> expr .
Axiom expr_WhyType : WhyType expr.
Existing Instance expr_WhyType.

(* Why3 assumption *)
Inductive stmt  :=
  | Sskip : stmt 
  | Sassign : ident -> expr -> stmt 
  | Sseq : stmt -> stmt -> stmt 
  | Sif : expr -> stmt -> stmt -> stmt 
  | Swhile : expr -> stmt -> stmt .
Axiom stmt_WhyType : WhyType stmt.
Existing Instance stmt_WhyType.

Axiom check_skip : forall (s:stmt), (s = Sskip) \/ ~ (s = Sskip).

(* Why3 assumption *)
Definition state  := (map.Map.map ident Z).

(* Why3 assumption *)
Definition eval_bin(x:Z) (op:operator) (y:Z): Z :=
  match op with
  | Oplus => (x + y)%Z
  | Ominus => (x - y)%Z
  | Omult => (x * y)%Z
  end.

(* Why3 assumption *)
Fixpoint eval_expr(s:(map.Map.map ident Z)) (e:expr) {struct e}: Z :=
  match e with
  | (Econst n) => n
  | (Evar x) => (map.Map.get s x)
  | (Ebin e1 op e2) => (eval_bin (eval_expr s e1) op (eval_expr s e2))
  end.

(* Why3 assumption *)
Inductive one_step : (map.Map.map ident Z) -> stmt -> (map.Map.map ident Z)
  -> stmt -> Prop :=
  | one_step_assign : forall (s:(map.Map.map ident Z)) (x:ident) (e:expr),
      (one_step s (Sassign x e) (map.Map.set s x (eval_expr s e)) Sskip)
  | one_step_seq : forall (s:(map.Map.map ident Z)) (s':(map.Map.map ident
      Z)) (i1:stmt) (i1':stmt) (i2:stmt), (one_step s i1 s' i1') ->
      (one_step s (Sseq i1 i2) s' (Sseq i1' i2))
  | one_step_seq_skip : forall (s:(map.Map.map ident Z)) (i:stmt),
      (one_step s (Sseq Sskip i) s i)
  | one_step_if_true : forall (s:(map.Map.map ident Z)) (e:expr) (i1:stmt)
      (i2:stmt), (~ ((eval_expr s e) = 0%Z)) -> (one_step s (Sif e i1 i2) s
      i1)
  | one_step_if_false : forall (s:(map.Map.map ident Z)) (e:expr) (i1:stmt)
      (i2:stmt), ((eval_expr s e) = 0%Z) -> (one_step s (Sif e i1 i2) s i2)
  | one_step_while_true : forall (s:(map.Map.map ident Z)) (e:expr) (i:stmt),
      (~ ((eval_expr s e) = 0%Z)) -> (one_step s (Swhile e i) s (Sseq i
      (Swhile e i)))
  | one_step_while_false : forall (s:(map.Map.map ident Z)) (e:expr)
      (i:stmt), ((eval_expr s e) = 0%Z) -> (one_step s (Swhile e i) s Sskip).



(* Why3 goal *)
Theorem progress : forall (s:(map.Map.map ident Z)) (i:stmt),
  (~ (i = Sskip)) -> exists s':(map.Map.map ident Z), exists i':stmt,
  (one_step s i s' i').
Proof.
intros s i Hskip.
induction i.

(* case i=skip *)
intuition.

(* case i=assign *)
exists (Map.set s i (eval_expr s e)).
exists Sskip.
constructor.

(* case i=seq *)
destruct (check_skip i1).

  subst i1.
  exists s. exists i2. constructor.

  elim (IHi1 H); clear H IHi1 IHi2.
  intros s1 H2; elim H2; clear H2. intros i1' Hind.
  exists s1. exists (Sseq i1' i2). constructor; auto.

(* case i=if *)
destruct (Z_eq_dec (eval_expr s e) 0%Z).
exists s. exists i2. constructor. auto.
exists s. exists i1. constructor. auto.

(* case i=while *)
destruct (Z_eq_dec (eval_expr s e) 0%Z).
exists s. exists Sskip. constructor. auto.
exists s. eexists. econstructor. auto.
Qed.


