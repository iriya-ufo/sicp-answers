;; ex2.34.scm

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

;; x = 2 で 1 + 3x + 5x^3 + x^5 の計算
(horner-eval 2 (list 1 3 0 5 0 1))
