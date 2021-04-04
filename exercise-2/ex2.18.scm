;; ex2.18.scm

(define (reverse list)
  (define (reverse-iter rev-list list)
    (if (null? list)
	rev-list
	(reverse-iter (cons (car list) rev-list) (cdr list))))
  (reverse-iter '() list))

(reverse (list 1 4 9 16 25))
