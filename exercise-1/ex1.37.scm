(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (cont-frac n d k)
  (define (combiner x cf)
    (/ (n x) (+ (d x) cf)))
  (define (term i) i)
  (define (next i) (+ i 1))
  (accumulate combiner (/ (n k) (d k)) term 1 next (- k 1)))

;; kの値が11以上で4桁の精度の近似を得る

;; b. 反復的プロセス
(define (accumulate-iter combiner null-value term a next b)
  (if (> a b)
      null-value
      (accumulate-iter combiner (combiner null-value (term a)) term (next a) next b)))

(define (cont-frac-iter n d k)
  (define (combiner cf x)
    (/ (n x) (+ (d x) cf)))
  (define (term i) (+ (- k i) 1))
  (define (next i) (+ i 1))
  (accumulate-iter combiner (/ (n k) (d k)) term 1 next (- k 1)))

;; 愚直な実装
(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n 1)
         (+ (d 1) (cont-frac (lambda (i) (n (+ i 1)))
                             (lambda (i) (d (+ i 1)))
                             (- k 1))))))

;; n = d = 1, k = 3 の場合
(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 3)
;; => 0.66666 = 2/3

;; 黄金比の逆数
(- (/ (- 1 (sqrt 5))
      2))
;; => 0.6180339887498949

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
;; => 0.6179775280898876

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)
;; => 0.6180555555555556

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)
;; => 0.6180257510729613
