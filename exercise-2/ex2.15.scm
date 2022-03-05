(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "Don't divide by an interval that spans zero.")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))

(define A (make-center-percent 6.0 0.1))

A
;; => (5.994 . 6.006)

;; これは代数的に等価な式である
;; A = A^2 / A

(div-interval (mul-interval A A) A)
;; => (5.982023976023975 . 6.018024024024025)

(div-interval (mul-interval A (mul-interval A A))
              (mul-interval A A))
;; => (5.970071880167784 . 6.030072120168217)
