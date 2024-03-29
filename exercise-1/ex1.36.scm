(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess count)
    (let ((next (f guess)))
      (cond ((close-enough? guess next) next)
            (else (print count ": " guess)
                  (try next (+ count 1))))))
  (try first-guess 0))

;; Non Average Damping
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             1.01)

;; Average Damping
(define (average x y) (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             1.01)
