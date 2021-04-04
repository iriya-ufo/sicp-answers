;; ex2.40.scm

(use srfi-1)				; filter

(define (square x) (* x x))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(accumulate append
	    '()
	    (map (lambda (i)
		   (map (lambda (j) (list i j))
			(enumerate-interval 1 (- i 1))))
		 (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))  
  (= n (smallest-divisor n)))

(define (divides? a b)
  (= 0 (remainder b a)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; unique-pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
	  (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

;; prime-sum-pairs with unique-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
	       (unique-pairs n))))

(prime-sum-pairs 6)
