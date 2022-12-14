(define (equal? list1 list2)
  (cond ((and (null? list1)
              (null? list2))
         #t)
        ((and (symbol? (car list1))
              (symbol? (car list2)))
         (and (eq? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        ((and (pair? (car list1))
              (pair? (car list2)))
         (and (equal? (car list1) (car list2))
              (equal? (cdr list1) (cdr list2))))
        (else #f)))

(equal? '(this is a list) '(this is a list))     ; => #t
(equal? '(this is a list) '(this (is a) list))   ; => #f
(equal? '(this (is a) list) '(this (is a) list)) ; => #t
