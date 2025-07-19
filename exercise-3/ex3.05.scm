(use srfi-27)

(define (estimate-integral p x1 x2 y1 y2 steps)
  (* (monte-carlo steps
                  (lambda ()
                    (let ((x (random-between x1 x2))
                          (y (random-between y1 y2)))
                      (p x y))))
     (rectangle-area x1 x2 y1 y2)))

(* 1.0 (estimate-integral in-unit-circle -1 1 -1 1 1000000)) ; => 3.142368

;;; 単位円
(define (in-unit-circle x y)
  (<= (+ (* x x) (* y y)) 1))

(define (rectangle-area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(rectangle-area 0 5 0 4)

;;; random-integer は整数を返す
;;; 整数は格子点となり都合が悪いため random-real を使用
;;; a, bの範囲のランダムな実数を返す
(define (random-between a b)
  (+ (* (- (random-real) 0.5) (- b a)) (/ (+ a b) 2)))

(random-between 10 100)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))
