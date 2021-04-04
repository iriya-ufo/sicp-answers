;; ex1.33.scm

(define (filtered-accumulate
	 combiner null-value term filter a next b)
  (cond ((> a b) null-value)
	((filter a)
	 (combiner
	  (term a)
	  (filtered-accumulate
	   combiner null-value term filter (next a) next b)))
	(else (filtered-accumulate
	       combiner null-value term filter (next a) next b))))


;; 素数テスト

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (prime? x)
  (define (smallest-divisor x)
    (find-divisor x 2))
  (define (find-divisor x test-divisor)
    (cond ((> (square test-divisor) x) x)
	  ((divides? test-divisor x) test-divisor)
	  (else (find-divisor x (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= x (smallest-divisor x)))

  
;; a. 区間a,bの素数の2乗の和

(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 square prime? a inc b))

(print (sum-of-squares-of-primes 10 13))
(print (sum-of-squares-of-primes 3 9))


;; b. nと互いに素で,nより小さい正の整数の積

(define (product-of-coprime-with-n n)
  (define (coprime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 identity coprime? 1 inc (- n 1)))

(print (product-of-coprime-with-n 5))
(print (product-of-coprime-with-n 11))
