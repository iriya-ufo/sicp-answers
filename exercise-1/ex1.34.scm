(define (f g)
  (g 2))

(define (square x) (* x x))

(f square)
;; => 4

(f (lambda (z) (* z (+ z 1))))
;; => 6

(f f)
;; *** ERROR: invalid application: (2 2)

;; (f f) を評価するとまず (f 2) となり、さらにこれを評価すると (2 2) となってしまい引数に作用させることができなくなる
