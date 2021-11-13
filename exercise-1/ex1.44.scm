(define (square x) (* x x))
(define (cube x) (* x x x))

(define dx 0.1)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define f square)
(define f abs)

((smooth f) 0)
;; => 0.06666666666666667

;; n-fold smoothed function
(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

((n-fold-smooth f 10) 0)
;; => 0.20477908177953905
