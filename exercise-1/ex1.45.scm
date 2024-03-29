(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (repeated f n)
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

;; x の n 乗根を m 回の平均緩和を利用して解く
(define (n-root-test x n m)
  (fixed-point ((repeated average-damp m)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 2乗根が何回で収束するか調べる
(n-root-test 9 2 1)                     ; 1回で収束
(n-root-test 16 2 1)                    ; 1回で収束

;; 3乗根が何回で収束するか調べる
(n-root-test 27 3 1)                    ; 1回で収束
(n-root-test 125 3 1)                   ; 1回で収束

;; 4乗根が何回で収束するか調べる
(n-root-test 16 4 1)                    ; 1回で収束しない
(n-root-test 16 4 2)                    ; 2回で収束

;; 5乗根が何回で収束するか調べる
(n-root-test 32 5 1)                    ; 1回で収束しない
(n-root-test 32 5 2)                    ; 2回で収束

;; 6乗根が何回で収束するか調べる
(n-root-test 64 6 2)                    ; 2回で収束

;; 7乗根が何回で収束するか調べる
(n-root-test 128 7 2)                   ; 2回で収束

;; 8乗根が何回で収束するか調べる
(n-root-test 256 8 2)                   ; 2回で収束しない
(n-root-test 256 8 3)                   ; 3回で収束

;; 15乗根が何回で収束するか調べる
(n-root-test 32768 15 3)                ; 3回で収束

;; 16乗根が何回で収束するか調べる
(n-root-test 65536 16 3)                ; 3回で収束しない
(n-root-test 65536 16 4)                ; 4回で収束

;; n 乗根を計算するのに必要な平均緩和の回数は log_2(n) 回
(define (n-root x n)
  (fixed-point ((repeated average-damp (floor (log n 2)))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-root 65536 16)
;; => 2.000000000076957
