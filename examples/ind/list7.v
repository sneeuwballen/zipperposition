
Require Import List.


Fixpoint rev {A:Type} (l:list A) : list A :=
  match l with
  | nil => nil
  | x :: tail => rev tail ++ x :: nil
  end.

Lemma app_assoc : forall A (l1:list A) l2 l3, l1 ++ (l2 ++ l3) = (l1 ++ l2) ++ l3.
Proof.
  intros A l1 l2 l3.
  induction l1.
  - trivial.
  - simpl.
    rewrite IHl1.
    trivial.
Qed. 

Lemma app_nil : forall A (l:list A), l ++ nil = l.
Proof.
  intros A l.
  induction l as [| x tail IHl].
  - trivial.
  - simpl. rewrite IHl. trivial.
Qed.

Lemma rev_append : forall A (l1:list A) l2, rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros A l1 l2.
  induction l1.
  - simpl.
    induction l2.
    * trivial.
    * simpl.
      rewrite IHl2.
      repeat (rewrite app_nil).
      trivial.
  - simpl.
    rewrite IHl1; simpl.
    rewrite <- app_assoc.
    trivial.
Qed.

Lemma rev_rev : forall A (l:list A), rev (rev l) = l.
Proof.
  intros A l.
  induction l.
  - trivial.
  - simpl.
    rewrite rev_append; simpl.
    rewrite IHl.
    trivial.
Qed.
