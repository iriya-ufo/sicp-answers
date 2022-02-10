;; Miller-Rabin test
(use srfi-27)

(define (expmod base exp m)
  ;; 1の非自明な平方根のチェック
  (define (non-trivial-sqrt-check x)
    (cond ((= x 1) 1)        ; 1の自明な平方根
          ((= x (- m 1)) 1)  ; 1の自明な平方根
          (else
           (if (= 1 (remainder (square x) m))
               0             ; 1の非自明な平方根が見つかったシグナル
               (remainder (square x) m)))))
  (cond ((= exp 0) 1)
        ((even? exp) (non-trivial-sqrt-check (expmod base (/ exp 2) m)))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

;; 20回試行する
(define (prime? n)
  (fast-prime? n 20))

(prime? 561)   ; => #f
(prime? 1105)  ; => #f
(prime? 1729)  ; => #f
(prime? 2465)  ; => #f
(prime? 2821)  ; => #f
(prime? 6601)  ; => #f

;; フェルマーテストをだましたカーマイケル数が、ミラーラビンテストはだませなかったことが示された
