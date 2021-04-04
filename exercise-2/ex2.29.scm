;; ex2.29.scm
;; # -*- coding: utf-8 -*-

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))


;; a.
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))


;; b.
(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left)
       (branch-weight right))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))


;; c.
(define (balanced? mobile)
  (if (balanced-mobile? mobile)
      #t
      #f))

(define (balanced-mobile? mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (let ((left-weight (balanced-branch? left)))
      (and left-weight
           (let ((right-weight (balanced-branch? right)))
             (and right-weight
                  (= (* (branch-length left) left-weight)
                     (* (branch-length right) right-weight))
                  (+ left-weight right-weight)))))))

(define (balanced-branch? branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (balanced-mobile? structure)
        structure)))


;; d.
;; right-branch , branch-structure ÇïœçXÇ∑ÇÍÇŒÇÊÇ¢
