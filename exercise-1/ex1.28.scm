;; ex1.28.scm
;; 未完

;; Miller-Rabin test

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (define (check a)
    (if (= a 1)
	#t
	(and (try-it a)
	     (check (- a 1)))))
  (check (- n 1)))

(display "miller-rabin test")
(newline)
(display "Carmichael numbers:")
(newline)
(display "561,1105,1729,2465,2821,6601")
(newline)
(display (miller-rabin-test 561))
(display (miller-rabin-test 1105))
(display (miller-rabin-test 1729))
(display (miller-rabin-test 2465))
(display (miller-rabin-test 2821))
(display (miller-rabin-test 6601))
(display (miller-rabin-test 341550071728321))
(newline)
