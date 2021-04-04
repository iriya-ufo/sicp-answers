;; ex2.09.scm

;; (p ± w[p]) + (q ± w[q]) → (p + q) ± (w[p] + w[q])
;; (p ± w[p]) - (q ± w[q]) → (p - q) ± (w[p] + w[q])

;; したがって,2つの区間の和(または差)の幅は,2つの区間のそれぞれの幅のみの関数である

;; (p ± w[p])・(q ± w[q]) → (p・q) ± (p・w[q] + q・w[p] + w[q]・w[p])
;; (p ± w[p])／(q ± w[q]) → (p・q ±(逆) p・w[q] ± q・w[p] - w[p]・w[q]) / (q^2 - w^2[q])

;; したがって,2つの区間の積(または商)の幅は,2つの区間のそれぞれの幅のみの関数にはならない



(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; add
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

;; sub
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;; mul
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

;; div
(define (div-interval x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;; width
(define (width x)
  (/ (- (upper-bound x) (lower-bound x))
     2.0))

;; test
(define s (make-interval 1 4))
(define t (make-interval 2 6))

(width s)				; => 1.5
(width t)				; => 2.0

;; add-test
(add-interval s t)			; => (3 . 10)
(width (add-interval s t))		; => 3.5
(+ (width s) (width t))			; => 3.5

;; sub-test
(sub-interval s t)			; => (-5 . 2)
(width (sub-interval s t))		; => 3.5
(- (width s) (width t))			; => -0.5

;; mul-test
(mul-interval s t)			; => (2 . 24)
(width (mul-interval s t))		; => 11.0
(* (width s) (width t))			; => 3.0

;; div-test
(div-interval s t)			; => (0.1666 . 2.0)
(width (div-interval s t))		; => 0.91666
(/ (width s) (width t))			; => 0.75
