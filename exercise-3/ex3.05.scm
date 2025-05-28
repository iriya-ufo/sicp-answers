(use srfi-27)
(print (random-integer 2))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
