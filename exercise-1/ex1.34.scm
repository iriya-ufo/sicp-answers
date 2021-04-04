;; ex1.34.scm

(define (f g)
  (g 2))

(define (square x) (* x x))

(print (f square))

(print (f (lambda (z) (* z (+ z 1)))))

(print (f f))

;; (f f)を評価するとまず(f 2)となり、
;; さらにこれを評価すると(2 2)となってしまい引数に作用させることができなくなる。
