(define-fun-rec
  m ((x Int)) Int (ite (> x 100) (- x 10) (m (m (+ x 11)))))
(assert-not (forall ((n Int)) (=> (>= n 101) (= (m n) (- n 10)))))
(check-sat)
