(define right-split (split beside below))
(define up-split (split below beside))

(define (split f g)
  (define (rec painter n)
    (if (= n 0)
        painter
        (let ((smaller (rec painter (- n 1))))
          (f painter (g smaller smaller)))))
  rec)
