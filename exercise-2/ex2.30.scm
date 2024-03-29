(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree-with-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-with-map sub-tree)
             (square sub-tree)))
       tree))

(square-tree-with-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
