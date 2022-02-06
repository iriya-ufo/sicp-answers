(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(add-1 zero)
; =>
(lambda (f)
  (lambda (x)
    (f (
        ((lambda (g) (lambda (y) y)) f) x))))
; => *1
(lambda (f)
  (lambda (x)
    (f (
        (lambda (y) y) x))))
; => *2
(lambda (f)
  (lambda (x) (f x)))


(add-1 one)
; =>
(lambda (f)
  (lambda (x)
    (f ((
        (lambda (g) (lambda (y) (g y))) f) x))))
; =>
(lambda (f)
  (lambda (x)
    (f (
        (lambda (y) (f y)) x))))
; =>
(lambda (f)
  (lambda (x)
    (f (f x))))

(define (inc x) (+ x 1))
((two inc) 3)
(((add-1 one) inc) 3)

(define (add-2 n)
  (lambda (f)
    (lambda (x)
      (f (f ((n f) x))))))

;; add-2 を展開
(add-1 (add-1 n))
((lambda (f) (lambda (x) (f ((n f) x))))
 ((lambda (g) (lambda (y) (g ((m f) y)))) m))

(((add-2 zero) inc) 10)

;; + の定義
(define (plus n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(((plus zero two) inc) 0)
;; => 2
(((plus two one) inc) 0)
;; => 3
(((plus three two) inc) 0)
;; => 5

;; * の定義
(define (mul n m)
  (lambda (f) (lambda (x) ((m (n f)) x))))

(((mul zero two) inc) 0)
;; => 0
(((mul one two) inc) 0)
;; => 2
(((mul three two) inc) 0)
;; => 6
