(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test-with-carmichael n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (all-check a n)
    (cond ((>= a n) #t)
          ((try-it a) (all-check (+ a 1) n))
          (else #f)))
  (all-check 2 n))

(fermat-test-with-carmichael 17)    ; => #t
(fermat-test-with-carmichael 500)   ; => #f

;; Carmichael number test
(fermat-test-with-carmichael 561)   ; => #t
(fermat-test-with-carmichael 1105)  ; => #t
(fermat-test-with-carmichael 1729)  ; => #t
(fermat-test-with-carmichael 2465)  ; => #t
(fermat-test-with-carmichael 2821)  ; => #t
(fermat-test-with-carmichael 6601)  ; => #t
