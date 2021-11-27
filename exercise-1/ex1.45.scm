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


;; 2回の平均緩和で収束しない
;; (print (n-root-test 2 8 2)) ;;収束しない
(n-root-test 3 8 2) ;;収束

;; n = 2^c とすると n 乗根を計算するのに必要な平均緩和の回数は c 回
(define (n-root n x)
  (define (damp-count m)
    (if (< m 2)
        0
        (+ 1 (damp-count (/ m 2)))))
  (fixed-point ((repeated average-damp (damp-count n))
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(n-root 4 2)
(n-root 16 2)
(n-root 6 8)
