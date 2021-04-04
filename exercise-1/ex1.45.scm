;; ex1.45.scm

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

(define (n-root-test m n x)
  (fixed-point ((repeated average-damp m)
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

;; 1���ʿ�Ѵ��¤Ǽ�«���ʤ�

;; (print (n-root-test 1 4 2)) ;;��«���ʤ�

(print (n-root-test 2 4 2)) ;;��«
(newline)

(print (n-root-test 2 5 2)) ;;��«
(newline)

(print (n-root-test 2 6 2)) ;;��«
(newline)

(print (n-root-test 2 7 2)) ;;��«
(newline)

;; 2���ʿ�Ѵ��¤Ǽ�«���ʤ�

;; (print (n-root-test 2 8 2)) ;;��«���ʤ�

(print (n-root-test 3 8 2)) ;;��«
(newline)


;; n = 2^c �Ȥ���� n �躬��׻�����Τ�ɬ�פ�ʿ�Ѵ��¤β���� c ��

(define (n-root n x)
  (define (damp-count m)
    (if (< m 2)
	0
	(+ 1 (damp-count (/ m 2)))))
  (fixed-point ((repeated average-damp (damp-count n))
		(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))

(print (n-root 4 2))
(print (n-root 16 2))
(print (n-root 6 8))
