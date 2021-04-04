;; ex2.01.scm

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))


;; ��ο��򰷤���褦make-rat�β���

(define (make-rat n d)
  (let ((cfn (abs n))
        (cfd (abs d)))
    (let ((g (gcd cfn cfd)))
      (cons (if (negative? (* n d))
                (* -1 (/ cfn g))
                (/ cfn g))
            (/ cfd g)))))

(define -one-half (make-rat -1 2))
(print-rat -one-half)

(define one-third (make-rat 1 3))
(print-rat one-third)

(print-rat (add-rat -one-half one-third))
(print-rat (sub-rat -one-half one-third))
(print-rat (mul-rat -one-half one-third))
(print-rat (div-rat -one-half one-third))
