(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.
Require map.Map.

(* Why3 assumption *)
Inductive datatype  :=
  | TYunit : datatype 
  | TYint : datatype 
  | TYbool : datatype .
Axiom datatype_WhyType : WhyType datatype.
Existing Instance datatype_WhyType.

(* Why3 assumption *)
Inductive value  :=
  | Vvoid : value 
  | Vint : Z -> value 
  | Vbool : bool -> value .
Axiom value_WhyType : WhyType value.
Existing Instance value_WhyType.

(* Why3 assumption *)
Inductive operator  :=
  | Oplus : operator 
  | Ominus : operator 
  | Omult : operator 
  | Ole : operator .
Axiom operator_WhyType : WhyType operator.
Existing Instance operator_WhyType.

(* Why3 assumption *)
Definition ident  := Z.

Axiom refident : Type.
Parameter refident_WhyType : WhyType refident.
Existing Instance refident_WhyType.

Axiom eq_refident_dec : forall (x:refident) (y:refident), (x = y) \/
  ~ (x = y).

(* Why3 assumption *)
Inductive term  :=
  | Tvalue : value -> term 
  | Tvar : Z -> term 
  | Tderef : refident -> term 
  | Tbin : term -> operator -> term -> term .
Axiom term_WhyType : WhyType term.
Existing Instance term_WhyType.

(* Why3 assumption *)
Inductive fmla  :=
  | Fterm : term -> fmla 
  | Fand : fmla -> fmla -> fmla 
  | Fnot : fmla -> fmla 
  | Fimplies : fmla -> fmla -> fmla 
  | Flet : Z -> term -> fmla -> fmla 
  | Fforall : Z -> datatype -> fmla -> fmla .
Axiom fmla_WhyType : WhyType fmla.
Existing Instance fmla_WhyType.

(* Why3 assumption *)
Definition env  := (map.Map.map refident value).

(* Why3 assumption *)
Inductive list (a:Type) {a_WT:WhyType a} :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Axiom list_WhyType : forall (a:Type) {a_WT:WhyType a}, WhyType (list a).
Existing Instance list_WhyType.
Implicit Arguments Nil [[a] [a_WT]].
Implicit Arguments Cons [[a] [a_WT]].

(* Why3 assumption *)
Definition stack  := (list (Z* value)%type).

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

Parameter get_stack: Z -> (list (Z* value)%type) -> value.

Axiom get_stack_def : forall (i:Z) (pi:(list (Z* value)%type)),
  match pi with
  | Nil => ((get_stack i pi) = Vvoid)
  | (Cons (x, v) r) => ((x = i) -> ((get_stack i pi) = v)) /\ ((~ (x = i)) ->
      ((get_stack i pi) = (get_stack i r)))
  end.

Axiom get_stack_eq : forall (x:Z) (v:value) (r:(list (Z* value)%type)),
  ((get_stack x (Cons (x, v) r)) = v).

Axiom get_stack_neq : forall (x:Z) (i:Z) (v:value) (r:(list (Z*
  value)%type)), (~ (x = i)) -> ((get_stack i (Cons (x, v) r)) = (get_stack i
  r)).

(* Why3 assumption *)
Fixpoint eval_term(sigma:(map.Map.map refident value)) (pi:(list (Z*
  value)%type)) (t:term) {struct t}: value :=
  match t with
  | (Tvalue v) => v
  | (Tvar id) => (get_stack id pi)
  | (Tderef id) => (map.Map.get sigma id)
  | (Tbin t1 op t2) => (eval_bin (eval_term sigma pi t1) op (eval_term sigma
      pi t2))
  end.

(* Why3 assumption *)
Fixpoint eval_fmla(sigma:(map.Map.map refident value)) (pi:(list (Z*
  value)%type)) (f:fmla) {struct f}: Prop :=
  match f with
  | (Fterm t) => ((eval_term sigma pi t) = (Vbool true))
  | (Fand f1 f2) => (eval_fmla sigma pi f1) /\ (eval_fmla sigma pi f2)
  | (Fnot f1) => ~ (eval_fmla sigma pi f1)
  | (Fimplies f1 f2) => (eval_fmla sigma pi f1) -> (eval_fmla sigma pi f2)
  | (Flet x t f1) => (eval_fmla sigma (Cons (x, (eval_term sigma pi t)) pi)
      f1)
  | (Fforall x TYint f1) => forall (n:Z), (eval_fmla sigma (Cons (x,
      (Vint n)) pi) f1)
  | (Fforall x TYbool f1) => forall (b:bool), (eval_fmla sigma (Cons (x,
      (Vbool b)) pi) f1)
  | (Fforall x TYunit f1) => (eval_fmla sigma (Cons (x, Vvoid) pi) f1)
  end.

Parameter subst_term: term -> refident -> Z -> term.

Axiom subst_term_def : forall (e:term) (r:refident) (v:Z),
  match e with
  | ((Tvalue _)|(Tvar _)) => ((subst_term e r v) = e)
  | (Tderef x) => ((r = x) -> ((subst_term e r v) = (Tvar v))) /\
      ((~ (r = x)) -> ((subst_term e r v) = e))
  | (Tbin e1 op e2) => ((subst_term e r v) = (Tbin (subst_term e1 r v) op
      (subst_term e2 r v)))
  end.

Parameter vsubst_term: term -> Z -> term -> term.

Axiom vsubst_term_def : forall (e:term) (v:Z) (t:term),
  match e with
  | ((Tvalue _)|(Tderef _)) => ((vsubst_term e v t) = e)
  | (Tvar x) => ((v = x) -> ((vsubst_term e v t) = t)) /\ ((~ (v = x)) ->
      ((vsubst_term e v t) = e))
  | (Tbin e1 op e2) => ((vsubst_term e v t) = (Tbin (vsubst_term e1 v t) op
      (vsubst_term e2 v t)))
  end.

(* Why3 assumption *)
Fixpoint fresh_in_term(id:Z) (t:term) {struct t}: Prop :=
  match t with
  | (Tvalue _) => True
  | (Tvar v) => ~ (id = v)
  | (Tderef _) => True
  | (Tbin t1 _ t2) => (fresh_in_term id t1) /\ (fresh_in_term id t2)
  end.

(* Why3 goal *)
Theorem eval_subst_term : forall (sigma:(map.Map.map refident value))
  (pi:(list (Z* value)%type)) (e:term) (x:refident) (v:Z), (fresh_in_term v
  e) -> ((eval_term sigma pi (subst_term e x
  v)) = (eval_term (map.Map.set sigma x (get_stack v pi)) pi e)).
induction e; intros x v' H.

rewrite (subst_term_def (Tvalue v) x v'); auto.

rewrite (subst_term_def (Tvar z) x v'); auto.

generalize (subst_term_def (Tderef r) x v').
intros (h1,h2).
case (eq_refident_dec x r).
intro; subst r; rewrite h1; simpl; auto.
rewrite Map.Select_eq; auto.
intro; rewrite h2; simpl; auto.
rewrite Map.Select_neq; auto.

rewrite (subst_term_def (Tbin e1 o e2) x v'); simpl; auto.
elim H; intros.
rewrite IHe1; auto.
rewrite IHe2; auto.
Qed.


