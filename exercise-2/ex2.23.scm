(define (for-each proc list)
  (cond ((null? list) (newline))
	(else (proc (car list)) (for-each proc (cdr list)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
