;; ex2.20.scm

;; ドット末尾記法

;; (define (f x y . z) <body>)
;; (define (g . w) <body>)


;; lambda-version

;; (define f (lambda (x y . z) <body>))
;; (define g (lambda w <body>))


(define (f x y . z) (+ x y (car z)))
(define (g . w) (cdr w))

(f 1 2 3 4 5 6)
(g 1 2 3 4 5 6)


(define (same-parity x . z)
  (define even-odd-check (if (even? x) even? odd?))
  (define (filter check z)
    (cond ((null? z) z)
	  ((check (car z)) (cons (car z) (filter check (cdr z))))
	  (else (filter check (cdr z)))))
  (cons x (filter even-odd-check z)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
(same-parity 2)
