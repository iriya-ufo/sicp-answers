;; ex2.32.scm
;; 未完

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(print (subsets (list 1 2 3)))
