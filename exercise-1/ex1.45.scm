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

;; x �� n �躬�� m ���ʿ�Ѵ��¤����Ѥ��Ʋ�
(define (n-root-test x n m)
  (fixed-point ((repeated average-damp m)
                (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

;; 2�躬������Ǽ�«���뤫Ĵ�٤�
(n-root-test 9 2 1)                     ; 1��Ǽ�«
(n-root-test 16 2 1)                    ; 1��Ǽ�«

;; 3�躬������Ǽ�«���뤫Ĵ�٤�
(n-root-test 27 3 1)                    ; 1��Ǽ�«
(n-root-test 125 3 1)                   ; 1��Ǽ�«

;; 4�躬������Ǽ�«���뤫Ĵ�٤�
(n-root-test 16 4 1)                    ; 1��Ǽ�«���ʤ�
(n-root-test 16 4 2)                    ; 2��Ǽ�«

;; 5�躬������Ǽ�«���뤫Ĵ�٤�
(n-root-test 32 5 1)                    ; 1��Ǽ�«���ʤ�
(n-root-test 32 5 2)                    ; 2��Ǽ�«


;; 2���ʿ�Ѵ��¤Ǽ�«���ʤ�
;; (print (n-root-test 2 8 2)) ;;��«���ʤ�
(n-root-test 3 8 2) ;;��«

;; n = 2^c �Ȥ���� n �躬��׻�����Τ�ɬ�פ�ʿ�Ѵ��¤β���� c ��
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
