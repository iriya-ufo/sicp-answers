;; ex2.39.scm

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))
(reverse-right '(1 2 3 4 5))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))
(reverse-left '(1 2 3 4 5))
