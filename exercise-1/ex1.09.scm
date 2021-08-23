;; http://people.csail.mit.edu/jaffer/SLIB.html
(use slib)
(require 'trace)

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

(trace +)
(+ 3 4)
