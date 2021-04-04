;; ex1.32.scm

;; sum-cubes , factorial

(define (sum-cubes a b)
  (define (cube x) (* x x x))
  (define (inc x) (+ x 1))
  (sum cube a inc b))

(define (factorial n)
  (define (term i) i)
  (define (next i) (+ i 1))
  (product term 1 next n))


;; sum-function , product-function

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))


;; a. $B:F5"E*%W%m%;%9(B

(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner null-value term (next a) next b))))


;; b. $BH?I|E*%W%m%;%9(B

(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) term (next a) next b)))


;; $B$I$A$i$+$r%3%a%s%H%"%&%H$9$k(B
;; (define accumulate accumulate-rec)
;; (define accumulate accumulate-iter)

(print (sum-cubes 1 10))
(print (factorial 5))
