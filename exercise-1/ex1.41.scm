;; ex1.41.scm

;; doubleは引数として一引数の手続きを取り、
;; 受け取った手続きを二回作用させる手続きを返す

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

(print ((double inc) 2))
(print ((double square) 2))

(print (((double (double double)) inc) 5))
