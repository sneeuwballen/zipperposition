
Fixpoint minus (x:nat)(y:nat) : nat :=
  match x, y with
  | _, 0 => x
  | 0, S _ => 0
  | S x1, S y1 => minus x1 y1
  end.

Lemma minus_x_x : forall x, minus x x = 0.
Proof.
  intros x.
  induction x.
  - trivial.
  - simpl. rewrite IHx. trivial.
Qed.

Lemma minus_0 : forall x, minus x 0 = x.
Proof.
  induction x.
  - trivial.
  - trivial.
Qed.

Lemma plus_r : forall x, x + 0 = x.
Proof.
  intros x.
  induction x as [| x' IH].
  - trivial.
  - simpl. rewrite IH. trivial.
Qed.

Theorem minus_plus : forall x y, minus (x+y) y = x.
Proof.
  intros x y.
  generalize dependent x.
  induction y.
  - intros x. simpl. rewrite plus_r. rewrite minus_0. trivial.
  - intros x.
    rewrite <- plus_n_Sm.
    simpl.
    apply IHy.
Qed.
