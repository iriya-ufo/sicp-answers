;; ex2.35.scm

(define a-list (list 1 2 3 4))
(define b-list (list 5 6 7 8))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; 2.2.2節の count-leaves
(define (count-leaves2.2.2 x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves2.2.2 (car x))
		 (count-leaves2.2.2 (cdr x))))))

(count-leaves2.2.2 a-list)
(count-leaves2.2.2 (cons a-list b-list))
(count-leaves2.2.2 (append a-list b-list))

;; 再定義
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

(count-leaves a-list)
(count-leaves (cons a-list b-list))
(count-leaves (append a-list b-list))
