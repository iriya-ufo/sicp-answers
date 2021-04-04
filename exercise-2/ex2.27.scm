;; ex2.27.scm

(define x (list (list 1 2) (list 3 4)))
(print x)

(reverse x)

(define (deep-reverse x)
  (if (not (pair? x))
      x
      (reverse (map deep-reverse x))))

(deep-reverse x)
