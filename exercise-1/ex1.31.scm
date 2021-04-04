;; ex1.31.scm

;; a. 再帰的プロセス

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (identity x) x)
(define (inc x) (+ x 1))

(print (factorial 5))


;; πの近似式
;; π/4=(2*4*4*6*6*8...)/(3*3*5*5*7*7...)

(define (square x) (* x x))

(define (pi n)
  (define (term i)
    (/ (* (* 2 i) (* 2 (+ i 1)))
       (square (+ (* 2 i) 1))))
  (define (next i) (+ i 1))
  (* 4 (product term 1 next n)))

(print (pi 1000))


;; b. 反復的プロセス

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

(define (fact-iter n)
  (product-iter identity 1 inc n))

(print (fact-iter 5))
