
; expect: unsat

(declare-datatypes (a)
  ((list (nil) (cons (head a) (tail (list a))))))

(define-fun-rec
  (par (a)
    (filter
       ((x (=> a Bool)) (y (list a))) (list a)
       (match y
         (case nil (as nil (list a)))
         (case (cons z xs)
           (ite (@ x z) (cons z (filter x xs)) (filter x xs)))))))

(define-fun-rec
  (par (a)
    (length
      ((l (list a))) Int
      (match l
        (case nil 0)
        (case (cons x l2) (+ 1 (length l2)))))))

(assert-not
 (par (a)
  (forall ((p (=> a Bool)) (xs (list a)))
    (<= (length (filter p xs)) (length xs)))))

(check-sat)
