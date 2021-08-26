(use slib)
(require 'trace)

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(trace sine)
(sine 12.15)
;; CALL sine 12.15
;;   CALL sine 4.05
;;     CALL sine 1.3499999999999999
;;       CALL sine 0.44999999999999996
;;         CALL sine 0.15
;;         RETN sine 0.1495
;;       RETN sine 0.4351345505
;;     RETN sine 0.9758465331678772
;;   RETN sine -0.7895631144708228
;; RETN sine -0.39980345741334

;; => -0.39980345741334

;; a. trace の結果よりCALLされた回数なので5回
;; b. Θ (log(a))

;; 詳しい解説は以下を参照。
;; https://www.notion.so/1-15-ae116b9deaef498990722ffb5e7e0da9
