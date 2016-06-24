
Require Import List.
Require Import Nat.

Fixpoint sum (l:list nat) : nat :=
  match l with
  | nil => 0
  | x :: tail => x + sum tail
  end.

Lemma app_cons_app : forall A l1 (x:A) l2, (l1 ++ x :: nil) ++ l2 = l1 ++ (x :: l2).
Admitted.

Lemma plus_assoc : forall a b c, a + (b + c) = (a + b) + c.
Admitted.

Lemma plus_com : forall a b, a + b = b + a.
Admitted.

Lemma sum_app : forall l1 l2, sum (l1 ++ l2) = sum l1 + sum l2.
Proof.
  intros l1 l2.
  induction l1.
  - trivial.
  - simpl.
    rewrite IHl1.
    rewrite plus_assoc.
    trivial.
Qed.

Theorem goal : forall l, sum (rev l) = sum l.
Proof.
  intro l.
  induction l.
  - trivial.
  - simpl.
    rewrite sum_app. simpl.
    rewrite <- plus_n_O.
    rewrite IHl.
    rewrite plus_com.
    trivial.
Qed.