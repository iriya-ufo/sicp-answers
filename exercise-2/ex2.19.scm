(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))
(define jp-coins '(500 100 50 10 5 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

(cc 100 us-coins)
;; => 292

(cc 20 uk-coins)
;; => 293

(cc 100 jp-coins)
;; => 159

(define us-coins2 '(10 5 1 25 50))
(cc 100 us-coins2)
;; => 292

;; cc の答えにリスト coin-values の順番は影響をあたえない
;; 順番がかわっても評価させる硬貨が区別できればよいアルゴリズムだからである
;; 硬貨を評価していくプロセスの木構造を描けばわかりやすい
;; すなわち木構造の見ための形は変わっても評価されるものは同じだからである
