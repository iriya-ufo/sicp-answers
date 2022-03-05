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
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100.0)))
(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 6.0 0.1))
(define B (make-center-percent 3.0 0.2))

(par1 A B)                   ; => (1.9913488681757654 . 2.008682242990654)
(par2 A B)                   ; => (1.9966662216288384 . 2.003332889480692)
;; par1 と par2 を比べると par1 の方が真の値から遠い
;; これは r1, r2 という因子が par2 よりも数が多いため誤差が大きくなったためだと思う

(center (par1 A B))          ; => 2.00001555558321
(center (par2 A B))          ; => 1.9999995555547652
;; par1, par2 の center の結果はそれほど大きな差異はない

(percent (par1 A B))         ; => 0.433331000014004
(percent (par2 A B))         ; => 0.16666673333345916
;; par1, par2 の percent の結果は差異が大きくなっているように見えるが percent なので 1/100 するとそれほど大きな差異はない

(center (div-interval A A))  ; => 1.000002000002
(center (div-interval A B))  ; => 2.000012000048
;; A/A, A/B の center の結果だがおおむね問題ないように思える

(percent (div-interval A A)) ; => 0.199999800000211
(percent (div-interval A B)) ; => 0.29999940000119574
;; A/A, A/B の percent の結果は差異が大きくなっているように見えるが percent なので 1/100 するとそれほど大きな差異はない
