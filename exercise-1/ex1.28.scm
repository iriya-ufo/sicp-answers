;; Miller-Rabin test
(use srfi-27)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	       (remainder (non-trivial-square-check (expmod base (/ exp 2) m))
		                m))
	      (else
	       (remainder (* base (expmod base (- exp 1) m))
		                m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? n)
  (fast-prime? n 10))

(prime? 13)
(prime? 561)
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 341550071728321)
