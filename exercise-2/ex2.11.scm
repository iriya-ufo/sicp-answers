;; ex2.11.scm

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

(define (mul-interval x y)
  (let ((xl (lower-bound x))
	(xu (upper-bound x))
	(yl (lower-bound y))
	(yu (upper-bound y)))
    (cond ((positive? xl)
	   (cond ((positive? yl)
		  (make-interval (* xl yl)
				 (* xu yu)))
		 ((negative? yu)
		  (make-interval (* xu yl)
				 (* xl yu)))
		 (else
		  (make-interval (* xu yl)
				 (* xu yu)))))
	  ((negative? xu)
	   (cond ((positive? yl)
		  (make-interval (* xl yu)
				 (* xu yl)))
		 ((negative? yu)
		  (make-interval (* xu yu)
				 (* xl yl)))
		 (else
		  (make-interval (* xl yu)
				 (* xl yl)))))
	  (else
	   (cond ((positive? yl)
		  (make-interval (* xl yu)
				 (* xu yu)))
		 ((negative? yu)
		  (make-interval (* xu yl)
				 (* xl yl)))
		 (else			; 乗算が二回必要
		  (make-interval (min (* xl yu) (* xu yl))
				 (max (* xl yl) (* xu yu))))))
	  )))

;; test
(define x+ (make-interval 1 2))
(define x- (make-interval -6 -2))
(define x+- (make-interval -1 3))

(mul-interval x+ x+)			; => (1 . 4)
(mul-interval x+ x-)			; => (-12 . -2)
(mul-interval x+ x+-)			; => (-2 . 6)

(mul-interval x- x+)			; => (-12 . -2)
(mul-interval x- x-)			; => (4 . 36)
(mul-interval x- x+-)			; => (-18 . 6)

(mul-interval x+- x+)			; => (-2 . 6)
(mul-interval x+- x-)			; => (-18 . 6)
(mul-interval x+- x+-)			; => (-3 . 9)
