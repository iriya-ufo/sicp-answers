(use srfi-19)
(use srfi-27)

(define trials 10)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
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
  (display elapsed-time)
  #t)

(define (search-for-primes from n)
  (cond ((= n 0) (newline) 'done)
        ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
        (else (search-for-primes (+ from 2) n))))

(define (search n)
  (display (search-for-primes n 1))
  (newline))

;; Louis Program
(search 1000000)
;; 1000001
;; 1000003 *** #<time-duration 2.790220000>
;; done

;; Square Program
(search 1000000)
;; 1000001
;; 1000003 *** #<time-duration 0.000046000>
;; done
