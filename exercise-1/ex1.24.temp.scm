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
    (cond ((= times 0) #t)
                  ((fermat-test n) (fast-prime? n (- times 1)))
                          (else #f)))

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
    (display (search-for-primes n 3))
      (newline))

(search (expt 10 27))
(search (expt 10 36))
