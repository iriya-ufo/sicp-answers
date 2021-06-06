;;; 改善前のコード
(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 2.0)    ;; => 1.4142156862745097
(sqrt 9.0)    ;; => 3.00009155413138
(sqrt 1.0e+2) ;; => 10.000000000139897
(sqrt 1.0e-2) ;; => 0.10032578510960605
(sqrt 1.0e-4) ;; => 0.03230844833048122
;; 1.0e-4 という小さい数では正しい結果が得られない

(sqrt 1.0e+32) ;; => 1.0e+16
(sqrt 1.0e+52) ;; => 評価が終わらない
;; 1.0e+52 という大きい数では評価が終わらない


;;; 改善後のコード
;;; 予測値の前後の値を除算して比較する
(define (good-enough? old-guess new-guess x)
  (< (abs (- 1.0 (/ old-guess new-guess))) 0.001))

(define (sqrt-iter old-guess new-guess x)
  (if (good-enough? old-guess new-guess x)
      new-guess
      (sqrt-iter new-guess (improve new-guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x x))

(sqrt 1.0e-2)  ;; => 0.10000000000139897
(sqrt 1.0e-4)  ;; => 0.010000000025490743
(sqrt 1.0e-32) ;; => 1.0000000000006072e-16
;; 先ほどよりずっと小さな数でも動作している

(sqrt 1.0e+32)  ;; => 1.0000000000006072e16
(sqrt 1.0e+52)  ;; => 1.0000000000353524e26
(sqrt 1.0e+256) ;; => 1.0000000000018174e128
;; 先ほどよりずっと大きな数でも動作している
