(define (same-parity x . z)
  (define even-odd-check (if (even? x) even? odd?))
  (define (filter check z)
    (cond ((null? z) z)
          ((check (car z)) (cons (car z) (filter check (cdr z))))
          (else (filter check (cdr z)))))
  (cons x (filter even-odd-check z)))

(same-parity 1 2 3 4 5 6 7)
;; => (1 3 5 7)

(same-parity 2 3 4 5 6 7)
;; => (2 4 6)

(same-parity 2)
;; => (2)
