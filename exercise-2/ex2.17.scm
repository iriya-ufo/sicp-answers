;; ex2.17.scm

(define (last-pair list)
  (define (last-pair-iter cdr-list list)
    (if (null? cdr-list)
	list
	(last-pair-iter (cdr cdr-list) (cdr list))))
  (last-pair-iter (cdr list) list))

(last-pair (list 23 72 149 34))
