

Inductive tree {A:Type} : Type :=
| E : tree
| N : @tree A -> A -> @tree A -> @tree A.

Fixpoint mem {A:Type} (x:A) (t:@tree A): Prop :=
  match t with
  | E => False
  | N l y r => x = y \/ mem x l \/ mem x r
  end.

Inductive sub_tree {A:Type} : @tree A -> @tree A -> Prop :=
  | Sub_same : forall t1 t2, t1 = t2 -> sub_tree t1 t2
  | Sub_left : forall t l x r, sub_tree t l -> sub_tree t (N l x r)
  | Sub_right : forall t l x r, sub_tree t r -> sub_tree t (N l x r).

Lemma sub_e : forall A (t:@tree A), sub_tree E t.
Proof.
  intros A t.
  induction t.
  - apply Sub_same. trivial.
  - apply Sub_left. assumption.
Qed.

Lemma node_invert :
  forall A (t:@tree A) l x r,
    sub_tree (N l x r) t -> sub_tree l t /\ sub_tree r t.
Proof.
  intros A t l x r H.
  induction H.
  - 
  induction t as [| tl IHl tx tr IHr].
  - destruct l; destruct r; split; try (inversion H).
  - induction H; split.
    * 

split.
    * apply Ind_right.
      have (IHItIHItr


Lemma trans :
  forall A (t1:@tree A) t2 t3,
    sub_tree t1 t2 -> sub_tree t2 t3 -> sub_tree t1 t3.
Proof.
  intros A t1 t2 t3 H1 H2.
  induction H1.
  - apply Ind_e.
  - assumption.
  - apply IHsub_tree.
    apply Ind_left.
  - assumption