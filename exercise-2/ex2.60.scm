(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define adjoin-set cons)

(define union-set append)

(define (remove-set-element x set)
  (define (remove-set-element-iter acc rest)
    (cond ((null? rest) acc)
          ((equal? x (car rest)) (append acc (cdr rest)))
          (else (remove-set-element-iter (adjoin-set (car rest) acc) (cdr rest)))))
  (remove-set-element-iter '() set))

(remove-set-element 3 '(1 2 2 3 3))     ; => (2 2 1 3)

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) (remove-set-element (car set1) set2))))
        (else (intersection-set (cdr set1) set2))))


(define (intersection-set set1 set2)
(cond ((or (null? set1) (null? set2)) '())
((element-of-set? (car set1) set2)
(cons (car set1) (intersection-set (cdr set1) set2)))
(else (intersection-set (cdr set1) set2))))


(intersection-set '(1 2 3 3) '(2 3 3 4))       ; => (2 3 3)
(intersection-set '(2 3 3 4) '(3 4 1 2 3 4 3)) ; => (2 3 3 4)
(intersection-set '(3 4 1 2 3 4 3) '(2 3 3 4)) ; => (3 4 2 3)
(intersection-set '(1 2 3 3) '(2 3 3 3 4))
(intersection-set '(2 3 3 3 4) '(1 2 3 3))

(union-set '(2 3 3 3 4) '(1 2 3 3))
(union-set '(1 2) '(2 3 3))              ; => (1 2 2 3 3)
