;; ex1.27.scm

;; fermat-test-version2

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-v2 n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (check a)
    (if (= a 1)
        #t
        (and (try-it a)
             (check (- a 1)))))
  (check (- n 1)))

(display "fermat-test-version2")
(newline)
(display "Carmichael numbers:")
(newline)
(display "561,1105,1729,2465,2821,6601")
(newline)
(display (fermat-test-v2 561))
(display (fermat-test-v2 1105))
(display (fermat-test-v2 1729))
(display (fermat-test-v2 2465))
(display (fermat-test-v2 2821))
(display (fermat-test-v2 6601))
(newline)

;; fermat-test $B$r$@$^$9$+$I$&$+D4$Y$k$K$O!"(Brandom $B<jB3$-$G$O$J$/!"(B
;; a < n $B$J$k$9$Y$F$N(B a $B$K$D$$$F3N$+$a$kI,MW$,$"$k!#(B
