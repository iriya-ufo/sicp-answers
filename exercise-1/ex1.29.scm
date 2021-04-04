;; ex1.29.scm

;; Simpson

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (g x)
    (+ (f x) (* 4 (f (+ x h))) (f (+ x (* 2 h)))))
  (define (add x) (+ x (* 2 h)))
  (* (/ h 3) 
     (sum g a add b)))

(print (integral cube 0 1 100))
(print (integral cube 0 1 1000))
