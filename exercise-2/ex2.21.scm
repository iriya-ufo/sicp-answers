;; ex2.21.scm

(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list-with-map items)
  (map square items))

(square-list (list 1 2 3 4))
(square-list-with-map (list 1 2 3 4))
