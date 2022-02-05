(use srfi-19)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (search-for-primes from n)
  (cond ((= n 0) (newline))
        ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
        (else (search-for-primes (+ from 2) n))))

(define (search n)
  (search-for-primes n 3))

(search 1000)
;; => 1001
;; => 1003
;; => 1005
;; => 1007
;; => 1009 *** #<time-duration 0.000005000>
;; => 1011
;; => 1013 *** #<time-duration 0.000005000>
;; => 1015
;; => 1017
;; => 1019 *** #<time-duration 0.000004000>

(search 10000)
;; => 10001
;; => 10003
;; => 10005
;; => 10007 *** #<time-duration 0.000011000>
;; => 10009 *** #<time-duration 0.000011000>
;; => 10011
;; => 10013
;; => 10015
;; => 10017
;; => 10019
;; => 10021
;; => 10023
;; => 10025
;; => 10027
;; => 10029
;; => 10031
;; => 10033
;; => 10035
;; => 10037 *** #<time-duration 0.000028000>

(search 100000)
;; => 100001
;; => 100003 *** #<time-duration 0.000042000>
;; => 100005
;; => 100007
;; => 100009
;; => 100011
;; => 100013
;; => 100015
;; => 100017
;; => 100019 *** #<time-duration 0.000033000>
;; => 100021
;; => 100023
;; => 100025
;; => 100027
;; => 100029
;; => 100031
;; => 100033
;; => 100035
;; => 100037
;; => 100039
;; => 100041
;; => 100043 *** #<time-duration 0.000034000>
