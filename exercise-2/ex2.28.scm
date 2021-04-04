;; ex2.28.scm

(define x (list (list 1 2) (list 3 4)))
(print x)

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(fringe x)
(fringe (list x x))
