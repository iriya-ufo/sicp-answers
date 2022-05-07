(define x (list (list 1 2) (list 3 4)))
x
;; => ((1 2) (3 4))

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(fringe x)
;; => (1 2 3 4)

(fringe (list x x))
;; => (1 2 3 4 1 2 3 4)

(fringe (list x (list x x)))
;; => (1 2 3 4 1 2 3 4 1 2 3 4)
