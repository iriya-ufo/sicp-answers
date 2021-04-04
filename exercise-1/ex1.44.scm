;; ex1.44.scm

(define (square x) (* x x))
(define (cube x) (* x x x))

(define dx 0.000001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
		 3)))


;; 関数 f の定義

(define f square)
;;(define f cube)
;;(define f hoge)

(print ((smooth f) 3))


;; n-fold smoothed function

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

(print ((n-fold-smooth f 10) 3))
