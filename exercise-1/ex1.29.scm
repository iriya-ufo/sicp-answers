(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (next x)
    (+ x (* 2 h)))
  (* (/ h 3)
     (+ (f a)
        (* 4 (sum f (+ a h) next b))
        (* 2 (sum f (+ a (* 2 h)) next (- b h)))
        (f b))))

(simpson cube 0 1.0 100)
;; => 0.2500000000000004

(simpson cube 0 1.0 1000)
;; => 0.25000000000000083
