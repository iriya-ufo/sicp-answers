(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3))

(define ls '(() (3)))
ls
(map f rest)
(map f ls)

rest_3 = '(() (3))
(define rest_3 '(() (3)))

(cons 2 '())                            ; => (2)
(cons 2 '(3))                           ; => (2 3)

(append rest_3 '((2) (2 3)))            ; => (() (3) (2) (2 3))

rest_2 = '(() (3) (2) (2 3))
(define rest_2 '(() (3) (2) (2 3)))

(cons 1 '())                            ; => (1)
(cons 1 '(3))                           ; => (1 3)
(cons 1 '(2))                           ; => (1 2)
(cons 1 '(2 3))                         ; => (1 2 3)

(append rest_2 '((1) (1 3) (1 2) (1 2 3))) ; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
