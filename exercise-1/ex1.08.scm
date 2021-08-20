(define (cube-root-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess x)
      new-guess
      (cube-root-iter new-guess (improve new-guess x) x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess))
     3.0))

(define (good-enough? old-guess new-guess x)
  (< (abs (- 1.0 (/ old-guess new-guess))) 0.001))

(define (cube-root x)
  (cube-root-iter 1.0 x x))

(cube-root 27)
;; => 3.0000000017936714

(cube-root (* 1.0e-6 1.0e-6 1.0e-6))
;; => 1.0000000059460973e-6

(cube-root (* 1.0e+54 1.0e+54 1.0e+54))
;; => 1.0000000001302823e54
