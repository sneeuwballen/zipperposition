; Rotate expressed using a snoc instead of append
; weakening of  tip-benchmarks/tip2015/rotate_snoc_self.smt2

; expect: UNSAT

(declare-datatypes (a)
  ((list (nil) (cons (head a) (tail (list a))))))
(declare-datatypes () ((Nat (Z) (S (p Nat)))))
(define-fun-rec
  (par (a)
    (snoc
       ((x a) (y (list a))) (list a)
       (match y
         (case nil (cons x (as nil (list a))))
         (case (cons z ys) (cons z (snoc x ys)))))))
(define-fun-rec
  (par (a)
    (rotate
       ((x Nat) (y (list a))) (list a)
       (match x
         (case Z y)
         (case (S z)
           (match y
             (case nil (as nil (list a)))
             (case (cons x2 x3) (rotate z (snoc x2 x3)))))))))
(define-fun-rec
  (par (a)
    (append
       ((x (list a)) (y (list a))) (list a)
       (match x
         (case nil y)
         (case (cons z xs) (cons z (append xs y)))))))


; goal is weakened because we ask for `exists xs`
(assert-not
  (par (a)
    (forall ((n Nat))
      (exists ((xs (list a)))
        (= (rotate n (append xs xs))
         (append (rotate n xs) (rotate n xs)))))))
(check-sat)
