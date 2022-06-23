(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;; a. �}��Ԃ��Z���N�^�Ǝ}�̍\���v�f
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

;;; b. ���r�[���̑��d�ʂ�Ԃ��葱��
(define (total-weight mobile)
  (let ((left-b (left-branch mobile))
        (right-b (right-branch mobile)))
    (+ (branch-weight left-b)
       (branch-weight right-b))))

(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (not (pair? structure))
        structure
        (total-weight structure))))

(total-weight m4)                       ; => 20
(total-weight m3)                       ; => 30
(total-weight m2)                       ; => 30
(total-weight m1)                       ; => 60

;;; c. �o�����X�����Ă��邩�m�F����葱��
(define (balanced? mobile)
  (let ((left-b (left-branch mobile))
        (right-b (right-branch mobile)))
    (let ((left-b-w (balanced-branch-weight left-b))
          (right-b-w (balanced-branch-weight right-b)))
      (and (= (* (branch-length left-b) left-b-w)
              (* (branch-length right-b) right-b-w))))))

;;; �u�����`�̍\�����d�肾�����ꍇ�́A�d�ʂ�Ԃ�
;;; �u�����`�̍\�������r�[���������ꍇ�́A�o�����X���v�Z����葱�����Ă�
(define (balanced-branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
        (balanced? structure)
        structure)))

(balanced? m1)
(balanced? m2)
(balanced? m3)
(balanced? m4)

;;; d. ���r�[���̕\����ύX����
(define (make-mobile left right) (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

;;; not balanced mobile
(define l4 (make-branch 1 10))
(define r4 (make-branch 1 10))
(define m4 (make-mobile l4 r4))

(define l3 (make-branch 1 m4))
(define r3 (make-branch 1 10))
(define m3 (make-mobile l3 r3))

(define l2 (make-branch 1 10))
(define r2 (make-branch 1 20))
(define m2 (make-mobile l2 r2))

(define l1 (make-branch 2 m2))
(define r1 (make-branch 2 m3))
(define m1 (make-mobile l1 r1))

;;; balanced mobile
(define l4-b (make-branch 1 10))
(define r4-b (make-branch 1 10))
(define m4-b (make-mobile l4-b r4-b))

(define l3-b (make-branch 1 m4-b))
(define r3-b (make-branch 2 10))
(define m3-b (make-mobile l3-b r3-b))

(define l2-b (make-branch 2 10))
(define r2-b (make-branch 1 20))
(define m2-b (make-mobile l2-b r2-b))

(define l1-b (make-branch 2 m2-b))
(define r1-b (make-branch 2 m3-b))
(define m1-b (make-mobile l1-b r1-b))
