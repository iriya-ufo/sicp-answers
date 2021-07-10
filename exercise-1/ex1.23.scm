(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(use srfi-19)

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (current-time)))

(define (start-prime-test n start-time)
  (and (prime? n)
       (report-prime (time-difference (current-time) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from n)
  (cond ((= n 0) (newline))
        ((even? from) (search-for-primes (+ from 1) n))
        ((timed-prime-test from) (search-for-primes (+ from 2) (- n 1)))
        (else (search-for-primes (+ from 2) n))))

(define (search n)
  (search-for-primes n 3))

;; 問題1.23の改良バージョンの場合
(search 1000)
;; => 1009 *** #<time-duration 0.000003000>
;; => 1013 *** #<time-duration 0.000003000>
;; => 1019 *** #<time-duration 0.000003000>

(search 10000)
;; => 10007 *** #<time-duration 0.000008000>
;; => 10009 *** #<time-duration 0.000008000>
;; => 10037 *** #<time-duration 0.000008000>

(search 100000)
;; => 100003 *** #<time-duration 0.000098000>
;; => 100019 *** #<time-duration 0.000029000>
;; => 100043 *** #<time-duration 0.000019000>

(search 1000000)
;; => 1000003 *** #<time-duration 0.000062000>
;; => 1000033 *** #<time-duration 0.000062000>
;; => 1000037 *** #<time-duration 0.000062000>

;; 問題1.22の改良していないバージョンの場合
(search 1000)
;; => 1009 *** #<time-duration 0.000003000>
;; => 1013 *** #<time-duration 0.000004000>
;; => 1019 *** #<time-duration 0.000004000>

(search 10000)
;; => 10007 *** #<time-duration 0.000010000>
;; => 10009 *** #<time-duration 0.000011000>
;; => 10037 *** #<time-duration 0.000010000>

(search 100000)
;; => 100003 *** #<time-duration 0.000040000>
;; => 100019 *** #<time-duration 0.000040000>
;; => 100043 *** #<time-duration 0.000040000>

(search 1000000)
;; => 1000003 *** #<time-duration 0.000127000>
;; => 1000033 *** #<time-duration 0.000126000>
;; => 1000037 *** #<time-duration 0.000127000>

;; 各結果を比較してみると2倍速くなっているわけではないことがわかる
;; このようになる理由は (next test-divisor) 手続きにおいて、毎回 test-divisor が2かどうか評価されてしまうという無駄があるためである
;; そのためステップ数は純粋に半分になった訳ではない
