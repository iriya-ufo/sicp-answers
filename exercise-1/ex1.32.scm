;; 再帰プロセス
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))
(sum-integers 1 10)
;; => 55

(define (identity x) x)
(define (inc x) (+ x 1))
(define (factorial n)
  (product identity 1 inc n))
(factorial 5)
;; => 120

;; 反復プロセス
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate combiner (combiner null-value (term a)) term (next a) next b)))
