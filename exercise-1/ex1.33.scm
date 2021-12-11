(define (filtered-accumulate combiner null-value filter term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate combiner null-value filter term (next a) next b)))
        (else (filtered-accumulate combiner null-value filter term (next a) next b))))

(define (inc x) (+ x 1))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((= n 1) 1)
          ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

;; a.
(define (sum-of-squares-of-primes a b)
  (filtered-accumulate + 0 prime? square a inc b))

(sum-of-squares-of-primes 1 10)
;; => 87

(+ (square 2)
   (square 3)
   (square 5)
   (square 7))
;; => 87

;; b.
(define (product-of-coprime-with-n n)
  (define (coprime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 coprime? identity 1 inc (- n 1)))

(product-of-coprime-with-n 15)
;; => 896896

(* 1 2 4 7 8 11 13 14)
;; => 896896
