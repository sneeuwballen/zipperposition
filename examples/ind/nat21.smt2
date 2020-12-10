(declare-datatypes () ((Nat (Z) (S (p Nat)))))
(define-fun-rec
  ge
    ((x Nat) (y Nat)) Bool
    (match y
      (case Z true)
      (case (S z)
        (match x
          (case Z false)
          (case (S x2) (ge x2 z))))))
(assert-not
  (forall ((x Nat) (y Nat) (z Nat))
    (=> (ge x y) (=> (ge y z) (ge x z)))))
(check-sat)
