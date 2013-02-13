(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import BuiltIn.
Require BuiltIn.
Require int.Int.

(* Why3 assumption *)
.
(* Why3 assumption *)
.
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
Definition env  := (map refident value).

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
Fixpoint eval_term(sigma:(map refident value)) (pi:(list (Z* value)%type))
  (t:term) {struct t}: value :=
  match t with
  | (Tvalue v) => v
  | (Tvar id) => (get_stack id pi)
  | (Tderef id) => (get sigma id)
  | (Tbin t1 op t2) => (eval_bin (eval_term sigma pi t1) op (eval_term sigma
      pi t2))
  end.

(* Why3 assumption *)
Fixpoint eval_fmla(sigma:(map refident value)) (pi:(list (Z* value)%type))
  (f:fmla) {struct f}: Prop :=
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

Axiom eval_subst_term : forall (sigma:(map refident value)) (pi:(list (Z*
  value)%type)) (e:term) (x:refident) (v:Z), (fresh_in_term v e) ->
  ((eval_term sigma pi (subst_term e x v)) = (eval_term (set sigma x
  (get_stack v pi)) pi e)).

Axiom eval_vsubst_term : forall (sigma:(map refident value)) (pi:(list (Z*
  value)%type)) (e:term) (v:Z) (va:value), ((eval_term sigma pi
  (vsubst_term e v (Tvalue va))) = (eval_term sigma (Cons (v, va) pi) e)).

Axiom eval_term_change_free : forall (t:term) (sigma:(map refident value))
  (pi:(list (Z* value)%type)) (id:Z) (v:value), (fresh_in_term id t) ->
  ((eval_term sigma (Cons (id, v) pi) t) = (eval_term sigma pi t)).

(* Why3 assumption *)
Fixpoint fresh_in_fmla(id:Z) (f:fmla) {struct f}: Prop :=
  match f with
  | (Fterm e) => (fresh_in_term id e)
  | ((Fand f1 f2)|(Fimplies f1 f2)) => (fresh_in_fmla id f1) /\
      (fresh_in_fmla id f2)
  | (Fnot f1) => (fresh_in_fmla id f1)
  | (Flet y t f1) => (~ (id = y)) /\ ((fresh_in_term id t) /\
      (fresh_in_fmla id f1))
  | (Fforall y ty f1) => (~ (id = y)) /\ (fresh_in_fmla id f1)
  end.

(* Why3 assumption *)
Fixpoint subst(f:fmla) (x:refident) (v:Z) {struct f}: fmla :=
  match f with
  | (Fterm e) => (Fterm (subst_term e x v))
  | (Fand f1 f2) => (Fand (subst f1 x v) (subst f2 x v))
  | (Fnot f1) => (Fnot (subst f1 x v))
  | (Fimplies f1 f2) => (Fimplies (subst f1 x v) (subst f2 x v))
  | (Flet y t f1) => (Flet y (subst_term t x v) (subst f1 x v))
  | (Fforall y ty f1) => (Fforall y ty (subst f1 x v))
  end.

(* Why3 assumption *)
Fixpoint vsubst(f:fmla) (v:Z) (t:term) {struct f}: fmla :=
  match f with
  | (Fterm e) => (Fterm (vsubst_term e v t))
  | (Fand f1 f2) => (Fand (vsubst f1 v t) (vsubst f2 v t))
  | (Fnot f1) => (Fnot (vsubst f1 v t))
  | (Fimplies f1 f2) => (Fimplies (vsubst f1 v t) (vsubst f2 v t))
  | (Flet y t1 f1) => (Flet y (vsubst_term t1 v t) (vsubst f1 v t))
  | (Fforall y ty f1) => (Fforall y ty (vsubst f1 v t))
  end.

Axiom eval_subst : forall (f:fmla) (sigma:(map refident value)) (pi:(list (Z*
  value)%type)) (x:refident) (v:Z), (fresh_in_fmla v f) -> ((eval_fmla sigma
  pi (subst f x v)) <-> (eval_fmla (set sigma x (get_stack v pi)) pi f)).

Axiom eval_vsubst : forall (f:fmla) (sigma:(map refident value)) (pi:(list
  (Z* value)%type)) (v:Z) (va:value), (eval_fmla sigma pi (vsubst f v
  (Tvalue va))) <-> (eval_fmla sigma (Cons (v, va) pi) f).

Require Import Why3.
Ltac ae := why3 "alt-ergo" timelimit 2.

(* Why3 goal *)
Theorem eval_swap : forall (f:fmla) (sigma:(map refident value)) (pi:(list
  (Z* value)%type)) (id1:Z) (id2:Z) (v1:value) (v2:value), (~ (id1 = id2)) ->
  ((eval_fmla sigma (Cons (id1, v1) (Cons (id2, v2) pi)) f) <->
  (eval_fmla sigma (Cons (id2, v2) (Cons (id1, v1) pi)) f)).
induction f.
intros sigma pi id1 id2 v1 v2.

Qed.

