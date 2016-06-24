
Require Import List.

Lemma mem_append : forall {A:Type} (x:A) l1 l2, In x l1 -> In x (app l1 l2).
  intros a x l1 l2 H.
  induction l1.
  generalize dependent H.
  - intro H. inversion H.
  - simpl.
    destruct H.
    * rewrite -> H. simpl. left. trivial.
    * right. apply IHl1. assumption.
Qed.
