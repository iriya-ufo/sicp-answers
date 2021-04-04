;; ex2.33.scm

(define (square x) (* x x))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define a-list (list 1 2 3 4 5 6 7))
(define b-list (list 1 1 2 2 3 3 4))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(map square a-list)
(append a-list b-list)
(length b-list)
