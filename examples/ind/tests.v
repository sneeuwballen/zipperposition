
Inductive nat : Set :=
| Z : nat
| S : nat -> nat.

Fixpoint plus (a b:nat): nat :=
    match a with
    | Z => b
    | S a' => S (plus a' b)
    end.

Notation "A + B" := (plus A B).
Notation "0" := Z.

Lemma zero_plus_right : forall n:nat, n + 0 = n.
Proof.
    induction n as [| n' ].
    - reflexivity.
    - simpl.
      rewrite -> IHn'.
      reflexivity.
Qed.

Lemma succ_plus_right : forall n1 n2, n1 + S n2 = S (n1 + n2).
Proof.
    intros n1 n2.
    induction n1 as [| n1' ].
    - reflexivity.
    - simpl. rewrite -> IHn1'. reflexivity.
Qed.

Lemma plus_comm : forall n1 n2, n1 + n2 = n2 + n1.
Proof.
    intros n1 n2.
    induction n1 as [| n1' ].
    - rewrite -> zero_plus_right. reflexivity.
    - simpl. rewrite -> IHn1'. rewrite -> succ_plus_right. reflexivity.
Qed.

(* attempt to prove commutativity without intermediate lemmas *)
Lemma plus_comm2 : forall n1 n2, n1 + n2 = n2 + n1.
Proof.
    induction n1 as [| n1' ].
    - induction n2.
      + reflexivity.
      + simpl. rewrite <- IHn2. reflexivity.
    - induction n2.
      + simpl. rewrite -> (IHn1' 0). reflexivity.
      + simpl.
        rewrite -> (IHn1' (S n2)).
        rewrite <- IHn2. simpl.
        rewrite -> (IHn1' n2).
        reflexivity.
Qed.

Lemma plus_comm_right : forall n1 n2, n1 + n2 = n2 + n1 /\ n1 + S n2 = S (n1 + n2).
Proof.
  induction n1 as [| n1' ].
  - induction n2 as [| n2' ].
    + split; reflexivity.
    + destruct IHn2' as [H1 H2].
      simpl. split; [rewrite <- H1 | ]; reflexivity.
  - induction n2 as [| n2' ].
    + simpl. 
Abort. (* TODO *)

Fixpoint double (n:nat) :=
    match n with
    | Z => Z
    | S n' => S (S (double n'))
    end.

Lemma double_plus : forall n:nat, double n = n + n.
Proof.
    induction n as [| n' ].
    - reflexivity.
    - simpl.
      rewrite -> succ_plus_right. rewrite -> IHn'. reflexivity.
Qed.

