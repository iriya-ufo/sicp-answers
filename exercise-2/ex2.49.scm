;; a.
(define outline-painter-segment-list
  (list (make-segment (make-vect 0 0) (make-vect 1 0))
        (make-segment (make-vect 0 0) (make-vect 0 1))
        (make-segment (make-vect 1 0) (make-vect 1 1))
        (make-segment (make-vect 0 1) (make-vect 1 1))))

(define outline-painter (segments->painter outline-painter-segment-list))

;; b.
(define x-painter-segment-list
  (list (make-segment (make-vect 0 0) (make-vect 1 1))
        (make-segment (make-vect 0 1) (make-vect 1 0))))

;; c.
(define rhombus-painter-segment-list
  (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
        (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
        (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
        (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))

;; d.
;; skip
