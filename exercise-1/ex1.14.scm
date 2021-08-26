(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)
;; => 4

;; プロセスを図示する木は以下を参照
;; https://www.notion.so/1-14-1015ec9a0ce4499aa81fa26f2158b054

;; 両替する金額の増加に対する、このプロセスの増加オーダー
;; ステップ数 Θ (n^5)
;; 空間 Θ (n)
