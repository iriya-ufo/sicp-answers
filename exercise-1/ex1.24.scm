(use srfi-19)
(use srfi-27)

(define trials 10)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define true #t)
(define false #f)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (fast-prime? n trials)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from n)
  (cond ((= n 0) (newline))
        ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
        (else (search-for-primes (+ from 2) n))))

(define (search n)
  (search-for-primes n 3))

(search 1000)
;; => 1009 *** #<time-duration 0.000024000>
;; => 1013 *** #<time-duration 0.000055000>
;; => 1019 *** #<time-duration 0.000024000>

(search 10000)
;; => 10007 *** #<time-duration 0.000043000>
;; => 10009 *** #<time-duration 0.000041000>
;; => 10037 *** #<time-duration 0.000041000>

(search 100000)
;; => 100003 *** #<time-duration 0.000088000>
;; => 100019 *** #<time-duration 0.000049000>
;; => 100043 *** #<time-duration 0.000039000>

(search 1000000)
;; => 1000003 *** #<time-duration 0.000042000>
;; => 1000033 *** #<time-duration 0.000041000>
;; => 1000037 *** #<time-duration 0.000043000>

(search (expt 2 10))
;; => 1031 *** #<time-duration 0.000024000>
;; => 1033 *** #<time-duration 0.000023000>
;; => 1039 *** #<time-duration 0.000025000>

(search (expt 2 20))
;; => 1048583 *** #<time-duration 0.000055000>
;; => 1048589 *** #<time-duration 0.000071000>
;; => 1048601 *** #<time-duration 0.000052000>
