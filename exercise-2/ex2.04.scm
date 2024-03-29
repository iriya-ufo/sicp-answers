(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

;; 置換えモデル
;; (car (cons x y))
;; ((cons x y) (lambda (p q) p))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; cdr の定義
(define (cdr z)
  (z (lambda (p q) q)))

(define foo (cons 1 2))
(car foo)
;; => 1
(cdr foo)
;; => 2
