(define a-list (list 1 2 3 4))
(define b-list (list 5 6 7 8))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;; 2.2.2 節の count-leaves
(define (count-leaves-2.2.2 x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-2.2.2 (car x))
                 (count-leaves-2.2.2 (cdr x))))))

(count-leaves-2.2.2 a-list)                             ; => 4
(count-leaves-2.2.2 (cons a-list b-list))               ; => 8
(count-leaves-2.2.2 (cons a-list (list a-list b-list))) ; => 12

;;; 再定義
(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x)
                                       (count-leaves x)
                                       1))
                       t)))

(count-leaves a-list)
(count-leaves (cons a-list b-list))
(count-leaves (cons a-list (list a-list b-list)))
