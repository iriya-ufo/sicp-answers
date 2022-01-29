(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

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





;; (((add-1 zero) inc) 0)
;; (((add-1 zero) inc) 1)

;; ;; (add-1 zero)の置換え

;; ;; (add-1 (lambda (f) (lambda (x) x)))
;; ;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
;; ;; (lambda (f) (lambda (x) (f x)))


;; (define one (lambda (f) (lambda (x) (f x))))
;; ((one inc) 0)

;; (define two (lambda (f) (lambda (x) (f (f x)))))
;; ((two inc) 0)

;; (define three (lambda (f) (lambda (x) (f (f (f x))))))
;; ((three inc) 0)

;; ;; + の定義

;; (define (add n1 n2)
;;   (lambda (f) (lambda (x) ((n2 f) ((n1 f) x)))))

;; (print (((add two one) inc) 0))
;; (print (((add three two) inc) 0))


;; ;; * の定義

;; (define (mul n1 n2)
;;   (lambda (f) (lambda (x) ((n1 (n2 f)) x))))

;; (print (((mul one two) inc) 0))
;; (print (((mul three two) inc) 0))
