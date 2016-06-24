

Require Import List.

Fixpoint rev_append {A:Type} (l1:list A) (l2:list A) : list A :=
  match l1 with
  | nil => l2
  | x :: tail => rev_append tail (x :: l2)
  end.

Fixpoint rev {A:Type} (l:list A) : list A :=
  match l with
  | nil => nil
  | x :: tail => rev tail ++ x :: nil
  end.

Lemma app_nil : forall A (l:list A), l ++ nil = l.
Proof.
  intros A l.
  induction l as [| x tail IHl].
  - trivial.
  - simpl. rewrite IHl. trivial.
Qed.

Lemma app_cons_app : forall A (l1:list A) x l2, (l1 ++ x :: nil) ++ l2 = l1 ++ (x :: l2).
Proof.
  intros A l1 x l2.
  induction l1 as [| x1 tail1 IH1].
  - trivial.
  - simpl.
    rewrite IH1.
    trivial.
Qed.

Theorem goal : forall A (l1:list A) l2, rev_append l1 l2 = rev l1 ++ l2.
Proof.
  intros A l1 l2.
  generalize dependent l2.
  induction l1.
  - simpl. trivial.
  - intros l2.
    simpl.
    rewrite app_cons_app.
    rewrite IHl1.
    trivial.
Qed.

