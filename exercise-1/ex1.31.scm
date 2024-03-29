;; a.
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
  (product identity 1 inc n))

(factorial 1)  ; => 1
(factorial 2)  ; => 2
(factorial 3)  ; => 6
(factorial 4)  ; => 24
(factorial 5)  ; => 120

;; ウォリスの公式
(define (wallis-formula a b)
  (define (term x)
    (square (/ (* 2 x)
               (+ (* 2 x) 1))))
  (product term a inc b))

(* 2 (wallis-formula 1.0 1000.0))
;; => 0.001569619063483441

;; ウォリスの公式を式変形した問題に記載の公式
(define (wallis-formula a b)
  (define (term x)
    (/ (* (* 2 x) (* 2 (+ x 1)))
       (square (+ (* 2 x) 1))))
  (product term a inc b))

(* 4 (wallis-formula 1.0 1000.0))
;; => 3.142377365093882

;; b.
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
