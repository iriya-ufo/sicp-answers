;; ex2.06.scm

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define (inc x) (+ x 1))
(print ((zero inc) 0))
(print ((zero inc) 1))
(newline)

(print (((add-1 zero) inc) 0))
(print (((add-1 zero) inc) 1))
(newline)


;; (add-1 zero)の置換え

;; (add-1 (lambda (f) (lambda (x) x)))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; (lambda (f) (lambda (x) (f x)))


(define one (lambda (f) (lambda (x) (f x))))
(print ((one inc) 0))

(define two (lambda (f) (lambda (x) (f (f x)))))
(print ((two inc) 0))

(define three (lambda (f) (lambda (x) (f (f (f x))))))
(print ((three inc) 0))


;; + の定義

(define (add n1 n2)
  (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))

(print (((add two one) inc) 0))
(print (((add three two) inc) 0))


;; * の定義

(define (mul n1 n2)
  (lambda (f) (lambda (x) ((n1 (n2 f)) x))))

(print (((mul one two) inc) 0))
(print (((mul three two) inc) 0))
