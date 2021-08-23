;; http://people.csail.mit.edu/jaffer/SLIB.html
(use slib)
(require 'trace)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(trace +)
(+ 4 5)
;; trace の表示
;; CALL + 4 5
;;   CALL + 3 5
;;     CALL + 2 5
;;       CALL + 1 5
;;         CALL + 0 5
;;         RETN + 5
;;       RETN + 6
;;     RETN + 7
;;   RETN + 8
;; RETN + 9

;; 置換モデルによるプロセスの図示
;; (+ 4 5)
;; (inc (+ 3 5))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; (inc (inc (inc 6)))
;; (inc (inc 7))
;; (inc 8)
;; 9

;; プロセスは展開と縮約の形を取り、演算は遅延評価になっている。
;; これは線形再帰プロセスである。

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

(trace +)
(+ 4 5)
;; trace の表示
;; CALL + 4 5
;;   CALL + 3 6
;;     CALL + 2 7
;;       CALL + 1 8
;;         CALL + 0 9
;;         RETN + 9
;;       RETN + 9
;;     RETN + 9
;;   RETN + 9
;; RETN + 9

;; 置換モデルによるプロセスの図示
;; (+ 4 5)
;; (+ 3 6)
;; (+ 2 7)
;; (+ 1 8)
;; (+ 0 9)
;; 9

;; + の定義が再帰手続きにより呼び出されるが、固定の空間で実行されている。
;; これは線形反復プロセスである。
