
Require Import List.

Fixpoint rev_append {A:Type} (l1:list A) (l2:list A) : list A :=
  match l1 with
  | nil => l2
  | x :: tail => rev_append tail (x :: l2)
  end.

Definition rev {A:Type} (l:list A) : list A := rev_append l nil.

Lemma app_nil : forall A (l:list A), l ++ nil = l.
Proof.
  intros A l.
  induction l as [| x tail IHl].
  - trivial.
  - simpl. rewrite IHl. trivial.
Qed.

Lemma rev_append_bis :
  forall A (l1:list A) l2 l3,
    rev_append (rev_append l1 l2) l3 = rev_append l2 (l1 ++ l3).
Proof.
  intros A l1 l2 l3.
  simpl.
  generalize dependent l2.
  generalize dependent l3.
  induction l1 as [| x tail1].
  - intros. trivial.
  - intros l2 l3.
    simpl.
    rewrite IHtail1.
    trivial.
Qed.

Lemma rev_rev : forall A (l:list A), rev (rev l) = l.
Proof.
  intros A l.
  unfold rev.
  induction l.
  - trivial.
  - simpl.
    rewrite rev_append_bis.
    simpl.
    rewrite app_nil.
    trivial.
Qed.
