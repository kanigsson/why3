(* This file is generated by Why3's Coq driver *)
(* Beware! Only edit allowed sections below    *)
Require Import ZArith.
Require Import Rbase.
Require int.Int.

(* Why3 assumption *)
Definition implb(x:bool) (y:bool): bool := match (x,
  y) with
  | (true, false) => false
  | (_, _) => true
  end.

(* Why3 assumption *)
Inductive datatype  :=
  | TYunit : datatype 
  | TYint : datatype 
  | TYbool : datatype .

(* Why3 assumption *)
Inductive value  :=
  | Vvoid : value 
  | Vint : Z -> value 
  | Vbool : bool -> value .

(* Why3 assumption *)
Inductive operator  :=
  | Oplus : operator 
  | Ominus : operator 
  | Omult : operator 
  | Ole : operator .

(* Why3 assumption *)
Definition ident  := Z.

(* Why3 assumption *)
Inductive term  :=
  | Tvalue : value -> term 
  | Tvar : Z -> term 
  | Tderef : Z -> term 
  | Tbin : term -> operator -> term -> term .

(* Why3 assumption *)
Inductive fmla  :=
  | Fterm : term -> fmla 
  | Fand : fmla -> fmla -> fmla 
  | Fnot : fmla -> fmla 
  | Fimplies : fmla -> fmla -> fmla 
  | Flet : Z -> term -> fmla -> fmla 
  | Fforall : Z -> datatype -> fmla -> fmla .

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

Parameter const: forall (a:Type) (b:Type), b -> (map a b).
Set Contextual Implicit.
Implicit Arguments const.
Unset Contextual Implicit.

Axiom Const : forall (a:Type) (b:Type), forall (b1:b) (a1:a),
  ((get (const b1:(map a b)) a1) = b1).

(* Why3 assumption *)
Definition env  := (map Z value).

(* Why3 assumption *)
Inductive list (a:Type) :=
  | Nil : list a
  | Cons : a -> (list a) -> list a.
Set Contextual Implicit.
Implicit Arguments Nil.
Unset Contextual Implicit.
Implicit Arguments Cons.

(* Why3 assumption *)
Definition stack  := (list (Z* value)%type).

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
  | (_, _) => ((eval_bin x op y) = Vvoid)
  end.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint eval_term(sigma:(map Z value)) (pi:(list (Z* value)%type))
  (t:term) {struct t}: value :=
  match t with
  | (Tvalue v) => v
  | (Tvar id) => (get_stack id pi)
  | (Tderef id) => (get sigma id)
  | (Tbin t1 op t2) => (eval_bin (eval_term sigma pi t1) op (eval_term sigma
      pi t2))
  end.
Unset Implicit Arguments.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint eval_fmla(sigma:(map Z value)) (pi:(list (Z* value)%type))
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
Unset Implicit Arguments.

Parameter subst_term: term -> Z -> Z -> term.

Axiom subst_term_def : forall (e:term) (r:Z) (v:Z),
  match e with
  | ((Tvalue _)|(Tvar _)) => ((subst_term e r v) = e)
  | (Tderef x) => ((r = x) -> ((subst_term e r v) = (Tvar v))) /\
      ((~ (r = x)) -> ((subst_term e r v) = e))
  | (Tbin e1 op e2) => ((subst_term e r v) = (Tbin (subst_term e1 r v) op
      (subst_term e2 r v)))
  end.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint fresh_in_term(id:Z) (t:term) {struct t}: Prop :=
  match t with
  | (Tvalue _) => True
  | (Tvar v) => ~ (id = v)
  | (Tderef _) => True
  | (Tbin t1 _ t2) => (fresh_in_term id t1) /\ (fresh_in_term id t2)
  end.
Unset Implicit Arguments.

Axiom eval_subst_term : forall (sigma:(map Z value)) (pi:(list (Z*
  value)%type)) (e:term) (x:Z) (v:Z), (fresh_in_term v e) ->
  ((eval_term sigma pi (subst_term e x v)) = (eval_term (set sigma x
  (get_stack v pi)) pi e)).

Axiom eval_term_change_free : forall (t:term) (sigma:(map Z value)) (pi:(list
  (Z* value)%type)) (id:Z) (v:value), (fresh_in_term id t) ->
  ((eval_term sigma (Cons (id, v) pi) t) = (eval_term sigma pi t)).

(* Why3 assumption *)
Set Implicit Arguments.
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
Unset Implicit Arguments.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint subst(f:fmla) (x:Z) (v:Z) {struct f}: fmla :=
  match f with
  | (Fterm e) => (Fterm (subst_term e x v))
  | (Fand f1 f2) => (Fand (subst f1 x v) (subst f2 x v))
  | (Fnot f1) => (Fnot (subst f1 x v))
  | (Fimplies f1 f2) => (Fimplies (subst f1 x v) (subst f2 x v))
  | (Flet y t f1) => (Flet y (subst_term t x v) (subst f1 x v))
  | (Fforall y ty f1) => (Fforall y ty (subst f1 x v))
  end.
Unset Implicit Arguments.

Axiom eval_subst : forall (f:fmla) (sigma:(map Z value)) (pi:(list (Z*
  value)%type)) (x:Z) (v:Z), (fresh_in_fmla v f) -> ((eval_fmla sigma pi
  (subst f x v)) <-> (eval_fmla (set sigma x (get_stack v pi)) pi f)).

Axiom eval_swap : forall (f:fmla) (sigma:(map Z value)) (pi:(list (Z*
  value)%type)) (id1:Z) (id2:Z) (v1:value) (v2:value), (~ (id1 = id2)) ->
  ((eval_fmla sigma (Cons (id1, v1) (Cons (id2, v2) pi)) f) <->
  (eval_fmla sigma (Cons (id2, v2) (Cons (id1, v1) pi)) f)).

Axiom eval_change_free : forall (f:fmla) (sigma:(map Z value)) (pi:(list (Z*
  value)%type)) (id:Z) (v:value), (fresh_in_fmla id f) -> ((eval_fmla sigma
  (Cons (id, v) pi) f) <-> (eval_fmla sigma pi f)).

(* Why3 assumption *)
Inductive expr  :=
  | Evalue : value -> expr 
  | Ebin : expr -> operator -> expr -> expr 
  | Evar : Z -> expr 
  | Ederef : Z -> expr 
  | Eassign : Z -> expr -> expr 
  | Eseq : expr -> expr -> expr 
  | Elet : Z -> expr -> expr -> expr 
  | Eif : expr -> expr -> expr -> expr 
  | Eassert : fmla -> expr 
  | Ewhile : expr -> fmla -> expr -> expr .

(* Why3 assumption *)
Inductive one_step : (map Z value) -> (list (Z* value)%type) -> expr -> (map
  Z value) -> (list (Z* value)%type) -> expr -> Prop :=
  | one_step_assign_ctxt : forall (sigma:(map Z value)) (sigmaqt:(map Z
      value)) (pi:(list (Z* value)%type)) (piqt:(list (Z* value)%type)) (x:Z)
      (e:expr) (eqt:expr), (one_step sigma pi e sigmaqt piqt eqt) ->
      (one_step sigma pi (Eassign x e) sigmaqt piqt (Eassign x eqt))
  | one_step_assign_value : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (x:Z) (v:value), (one_step sigma pi (Eassign x
      (Evalue v)) (set sigma x v) pi (Evalue Vvoid))
  | one_step_seq_ctxt : forall (sigma:(map Z value)) (sigmaqt:(map Z value))
      (pi:(list (Z* value)%type)) (piqt:(list (Z* value)%type)) (e1:expr)
      (e1qt:expr) (e2:expr), (one_step sigma pi e1 sigmaqt piqt e1qt) ->
      (one_step sigma pi (Eseq e1 e2) sigmaqt piqt (Eseq e1qt e2))
  | one_step_seq_value : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (e:expr), (one_step sigma pi (Eseq (Evalue Vvoid) e)
      sigma pi e)
  | one_step_let_ctxt : forall (sigma:(map Z value)) (sigmaqt:(map Z value))
      (pi:(list (Z* value)%type)) (piqt:(list (Z* value)%type)) (id:Z)
      (e1:expr) (e1qt:expr) (e2:expr), (one_step sigma pi e1 sigmaqt piqt
      e1qt) -> (one_step sigma pi (Elet id e1 e2) sigmaqt piqt (Elet id e1qt
      e2))
  | one_step_let_value : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (id:Z) (v:value) (e:expr), (one_step sigma pi (Elet id
      (Evalue v) e) sigma (Cons (id, v) pi) e)
  | one_step_if_ctxt : forall (sigma:(map Z value)) (sigmaqt:(map Z value))
      (pi:(list (Z* value)%type)) (piqt:(list (Z* value)%type)) (e1:expr)
      (e1qt:expr) (e2:expr) (e3:expr), (one_step sigma pi e1 sigmaqt piqt
      e1qt) -> (one_step sigma pi (Eif e1 e2 e3) sigmaqt piqt (Eif e1qt e2
      e3))
  | one_step_if_true : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (e1:expr) (e2:expr), (one_step sigma pi
      (Eif (Evalue (Vbool true)) e1 e2) sigma pi e1)
  | one_step_if_false : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (e1:expr) (e2:expr), (one_step sigma pi
      (Eif (Evalue (Vbool false)) e1 e2) sigma pi e2)
  | one_step_assert : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (f:fmla), (eval_fmla sigma pi f) -> (one_step sigma pi
      (Eassert f) sigma pi (Evalue Vvoid))
  | one_step_while : forall (sigma:(map Z value)) (pi:(list (Z* value)%type))
      (cond:expr) (inv:fmla) (body:expr), (eval_fmla sigma pi inv) ->
      (one_step sigma pi (Ewhile cond inv body) sigma pi (Eif cond (Eseq body
      (Ewhile cond inv body)) (Evalue Vvoid))).

(* Why3 assumption *)
Inductive many_steps : (map Z value) -> (list (Z* value)%type) -> expr
  -> (map Z value) -> (list (Z* value)%type) -> expr -> Z -> Prop :=
  | many_steps_refl : forall (sigma:(map Z value)) (pi:(list (Z*
      value)%type)) (e:expr), (many_steps sigma pi e sigma pi e 0%Z)
  | many_steps_trans : forall (sigma1:(map Z value)) (sigma2:(map Z value))
      (sigma3:(map Z value)) (pi1:(list (Z* value)%type)) (pi2:(list (Z*
      value)%type)) (pi3:(list (Z* value)%type)) (e1:expr) (e2:expr)
      (e3:expr) (n:Z), (one_step sigma1 pi1 e1 sigma2 pi2 e2) ->
      ((many_steps sigma2 pi2 e2 sigma3 pi3 e3 n) -> (many_steps sigma1 pi1
      e1 sigma3 pi3 e3 (n + 1%Z)%Z)).

Axiom steps_non_neg : forall (sigma1:(map Z value)) (sigma2:(map Z value))
  (pi1:(list (Z* value)%type)) (pi2:(list (Z* value)%type)) (e1:expr)
  (e2:expr) (n:Z), (many_steps sigma1 pi1 e1 sigma2 pi2 e2 n) ->
  (0%Z <= n)%Z.

Axiom many_steps_seq : forall (sigma1:(map Z value)) (sigma3:(map Z value))
  (pi1:(list (Z* value)%type)) (pi3:(list (Z* value)%type)) (e1:expr)
  (e2:expr) (n:Z), (many_steps sigma1 pi1 (Eseq e1 e2) sigma3 pi3
  (Evalue Vvoid) n) -> exists sigma2:(map Z value), exists pi2:(list (Z*
  value)%type), exists n1:Z, exists n2:Z, (many_steps sigma1 pi1 e1 sigma2
  pi2 (Evalue Vvoid) n1) /\ ((many_steps sigma2 pi2 e2 sigma3 pi3
  (Evalue Vvoid) n2) /\ (n = ((1%Z + n1)%Z + n2)%Z)).

Axiom many_steps_let : forall (sigma1:(map Z value)) (sigma3:(map Z value))
  (pi1:(list (Z* value)%type)) (pi3:(list (Z* value)%type)) (id:Z) (e1:expr)
  (e2:expr) (v2:value) (n:Z), (many_steps sigma1 pi1 (Elet id e1 e2) sigma3
  pi3 (Evalue v2) n) -> exists sigma2:(map Z value), exists pi2:(list (Z*
  value)%type), exists v1:value, exists n1:Z, exists n2:Z, (many_steps sigma1
  pi1 e1 sigma2 pi2 (Evalue v1) n1) /\ ((many_steps sigma2 (Cons (id, v1)
  pi2) e2 sigma3 pi3 (Evalue v2) n2) /\ (n = ((1%Z + n1)%Z + n2)%Z)).

(* Why3 assumption *)
Definition valid_fmla(p:fmla): Prop := forall (sigma:(map Z value)) (pi:(list
  (Z* value)%type)), (eval_fmla sigma pi p).

(* Why3 assumption *)
Definition valid_triple(p:fmla) (e:expr) (q:fmla): Prop := forall (sigma:(map
  Z value)) (pi:(list (Z* value)%type)), (eval_fmla sigma pi p) ->
  forall (sigmaqt:(map Z value)) (piqt:(list (Z* value)%type)) (v:value)
  (n:Z), (many_steps sigma pi e sigmaqt piqt (Evalue v) n) ->
  (eval_fmla sigmaqt (Cons ((-1%Z)%Z, v) piqt) q).

(* Why3 assumption *)
Definition total_valid_triple(p:fmla) (e:expr) (q:fmla): Prop :=
  forall (sigma:(map Z value)) (pi:(list (Z* value)%type)), (eval_fmla sigma
  pi p) -> exists sigmaqt:(map Z value), exists piqt:(list (Z* value)%type),
  exists v:value, exists n:Z, (many_steps sigma pi e sigmaqt piqt (Evalue v)
  n) /\ (eval_fmla sigmaqt (Cons ((-1%Z)%Z, v) piqt) q).

Parameter set1 : forall (a:Type), Type.

Parameter mem: forall (a:Type), a -> (set1 a) -> Prop.
Implicit Arguments mem.

(* Why3 assumption *)
Definition infix_eqeq (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop :=
  forall (x:a), (mem x s1) <-> (mem x s2).
Implicit Arguments infix_eqeq.

Axiom extensionality : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (infix_eqeq s1 s2) -> (s1 = s2).

(* Why3 assumption *)
Definition subset (a:Type)(s1:(set1 a)) (s2:(set1 a)): Prop := forall (x:a),
  (mem x s1) -> (mem x s2).
Implicit Arguments subset.

Axiom subset_trans : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a))
  (s3:(set1 a)), (subset s1 s2) -> ((subset s2 s3) -> (subset s1 s3)).

Parameter empty: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments empty.
Unset Contextual Implicit.

(* Why3 assumption *)
Definition is_empty (a:Type)(s:(set1 a)): Prop := forall (x:a), ~ (mem x s).
Implicit Arguments is_empty.

Axiom empty_def1 : forall (a:Type), (is_empty (empty :(set1 a))).

Parameter add: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments add.

Axiom add_def1 : forall (a:Type), forall (x:a) (y:a), forall (s:(set1 a)),
  (mem x (add y s)) <-> ((x = y) \/ (mem x s)).

Parameter remove: forall (a:Type), a -> (set1 a) -> (set1 a).
Implicit Arguments remove.

Axiom remove_def1 : forall (a:Type), forall (x:a) (y:a) (s:(set1 a)), (mem x
  (remove y s)) <-> ((~ (x = y)) /\ (mem x s)).

Axiom subset_remove : forall (a:Type), forall (x:a) (s:(set1 a)),
  (subset (remove x s) s).

Parameter union: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments union.

Axiom union_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (union s1 s2)) <-> ((mem x s1) \/ (mem x s2)).

Parameter inter: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments inter.

Axiom inter_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (inter s1 s2)) <-> ((mem x s1) /\ (mem x s2)).

Parameter diff: forall (a:Type), (set1 a) -> (set1 a) -> (set1 a).
Implicit Arguments diff.

Axiom diff_def1 : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)) (x:a),
  (mem x (diff s1 s2)) <-> ((mem x s1) /\ ~ (mem x s2)).

Axiom subset_diff : forall (a:Type), forall (s1:(set1 a)) (s2:(set1 a)),
  (subset (diff s1 s2) s1).

Parameter choose: forall (a:Type), (set1 a) -> a.
Implicit Arguments choose.

Axiom choose_def : forall (a:Type), forall (s:(set1 a)), (~ (is_empty s)) ->
  (mem (choose s) s).

Parameter all: forall (a:Type), (set1 a).
Set Contextual Implicit.
Implicit Arguments all.
Unset Contextual Implicit.

Axiom all_def : forall (a:Type), forall (x:a), (mem x (all :(set1 a))).

(* Why3 assumption *)
Definition assigns(sigma:(map Z value)) (a:(set1 Z)) (sigmaqt:(map Z
  value)): Prop := forall (i:Z), (~ (mem i a)) -> ((get sigma
  i) = (get sigmaqt i)).

Axiom assigns_refl : forall (sigma:(map Z value)) (a:(set1 Z)),
  (assigns sigma a sigma).

Axiom assigns_trans : forall (sigma1:(map Z value)) (sigma2:(map Z value))
  (sigma3:(map Z value)) (a:(set1 Z)), ((assigns sigma1 a sigma2) /\
  (assigns sigma2 a sigma3)) -> (assigns sigma1 a sigma3).

Axiom assigns_union_left : forall (sigma:(map Z value)) (sigmaqt:(map Z
  value)) (s1:(set1 Z)) (s2:(set1 Z)), (assigns sigma s1 sigmaqt) ->
  (assigns sigma (union s1 s2) sigmaqt).

Axiom assigns_union_right : forall (sigma:(map Z value)) (sigmaqt:(map Z
  value)) (s1:(set1 Z)) (s2:(set1 Z)), (assigns sigma s2 sigmaqt) ->
  (assigns sigma (union s1 s2) sigmaqt).

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint expr_writes(e:expr) (w:(set1 Z)) {struct e}: Prop :=
  match e with
  | ((Evalue _)|((Evar _)|((Ederef _)|(Eassert _)))) => True
  | (Ebin e1 _ e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Eassign id _) => (mem id w)
  | (Eseq e1 e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Elet id e1 e2) => (expr_writes e1 w) /\ (expr_writes e2 w)
  | (Eif e1 e2 e3) => (expr_writes e1 w) /\ ((expr_writes e2 w) /\
      (expr_writes e3 w))
  | (Ewhile cond _ body) => (expr_writes cond w) /\ (expr_writes body w)
  end.
Unset Implicit Arguments.

Parameter fresh_from_fmla: fmla -> Z.

Axiom fresh : forall (f:fmla), (fresh_in_fmla (fresh_from_fmla f) f).

Parameter abstract_effects: expr -> fmla -> fmla.

(* Why3 assumption *)
Set Implicit Arguments.
Fixpoint wp(e:expr) (q:fmla) {struct e}: fmla :=
  match e with
  | (Evalue v) => (Flet (-1%Z)%Z (Tvalue v) q)
  | (Evar v) => (Flet (-1%Z)%Z (Tvar v) q)
  | (Ederef x) => (Flet (-1%Z)%Z (Tderef x) q)
  | (Eassert f) => (Fand f (Fimplies f q))
  | (Eseq e1 e2) => (wp e1 (wp e2 q))
  | (Elet id e1 e2) => (wp e1 (Flet id (Tvar (-1%Z)%Z) (wp e2 q)))
  | (Ebin e1 op e2) => let t1 := (fresh_from_fmla q) in let t2 :=
      (fresh_from_fmla (Fand (Fterm (Tvar t1)) q)) in let qqt :=
      (Flet (-1%Z)%Z (Tbin (Tvar t1) op (Tvar t2)) q) in let f := (wp e2
      (Flet t2 (Tvar (-1%Z)%Z) qqt)) in (wp e1 (Flet t1 (Tvar (-1%Z)%Z) f))
  | (Eassign x e1) => let id := (fresh_from_fmla q) in let qqt :=
      (Flet (-1%Z)%Z (Tvalue Vvoid) q) in (wp e1 (Flet id (Tvar (-1%Z)%Z)
      (subst qqt x id)))
  | (Eif e1 e2 e3) => let f := (Fand (Fimplies (Fterm (Tvar (-1%Z)%Z)) (wp e2
      q)) (Fimplies (Fnot (Fterm (Tvar (-1%Z)%Z))) (wp e3 q))) in (wp e1 f)
  | (Ewhile cond inv body) => (Fand inv (abstract_effects body (wp cond
      (Fand (Fimplies (Fand (Fterm (Tvar (-1%Z)%Z)) inv) (wp body inv))
      (Fimplies (Fand (Fnot (Fterm (Tvar (-1%Z)%Z))) inv) q)))))
  end.
Unset Implicit Arguments.

Require Why3.
Ltac ae := why3 "alt-ergo" timelimit 2.
Ltac ae10 := why3 "alt-ergo" timelimit 10.
Ltac cvc10 := why3 "cvc3" timelimit 10.

(* Why3 goal *)
Theorem wp_conj : forall (sigma:(map Z value)) (pi:(list (Z* value)%type))
  (e:expr) (p:fmla) (q:fmla), (eval_fmla sigma pi (wp e (Fand p q))) <->
  ((eval_fmla sigma pi (wp e p)) /\ (eval_fmla sigma pi (wp e q))).
induction e.
(* Evalue *)
ae.
(* Ebin *)
admit.
(* Evar *)
ae.
(* Ederef *)
ae.
(* Eassign *)
admit.
(* Eseq *)
admit.
(* Elet *)
admit.
(* Eif *)
admit.
(* Eassert *)
ae10.
(* Ewhile *)

Qed.


