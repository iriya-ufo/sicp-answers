;; ex2.37.scm
;; # -*- coding: utf-8 -*-

(define matrix-list (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define v '((2 3 2) (1 2 3) (3 3 4)))
(define w '((1 2 9) (2 3 1) (7 3 1)))

;; matrix-list
;; +-         -+
;; |  1 2 3 4  |
;; |  4 5 6 6  |
;; |  6 7 8 9  |
;; +-         -+

;; v
;; +-     -+
;; | 2 3 2 |
;; | 1 2 3 |
;; | 3 3 4 |
;; +-     -+

;; w
;; +-     -+
;; | 1 2 9 |
;; | 2 3 1 |
;; | 7 3 1 |
;; +-     -+

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))
(dot-product (list 1 2 3) (list 4 5 6))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(matrix-*-vector matrix-list (list 1 2 3 4))

(define (transpose mat)
  (accumulate-n cons '() mat))
(transpose matrix-list)


;; 僕のやり方
(define (matrix-*-matrix m n)
  (transpose (map (lambda (w) (matrix-*-vector m w)) n)))
(matrix-*-matrix matrix-list (transpose matrix-list))
(matrix-*-matrix matrix-list matrix-list)
(matrix-*-matrix v (transpose w))
(matrix-*-matrix v w)


;; SICP でのやり方
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (u) (matrix-*-vector cols u)) m)))
(matrix-*-matrix matrix-list (transpose matrix-list))
(matrix-*-matrix matrix-list matrix-list)
(matrix-*-matrix v (transpose w))
(matrix-*-matrix v w)


;; n x m * m x n で transpose を使用しているかどうかチェックする
(define (matrix-*-matrix m n)
  (define (calc m n)
    (let ((cols (transpose n)))
      (map (lambda (u) (matrix-*-vector cols u)) m)))
  (if (and (= (length m) (length (car m)))
	   (= (length n) (length (car n))))
      (calc m n)
      (begin
	(print "Does it use 'transpose' func?")
	(calc m n))))
(matrix-*-matrix matrix-list (transpose matrix-list))
(matrix-*-matrix matrix-list matrix-list)
(matrix-*-matrix v (transpose w))
(matrix-*-matrix v w)


;; 武藤さんのやり方
(define (valid-matrix? m)
  (apply = (map length m)))

(define (matrix-rows m)
  (length m))

(define (matrix-cols m)
  (length (car m)))

(define (matrix-*-matrix m n)
  (define (calc m n)
    (let ((cols (transpose n)))
      (map (lambda (u) (matrix-*-vector cols u)) m)))
  (if (and (valid-matrix? m)
           (valid-matrix? n)
           (= (matrix-cols m)
              (matrix-rows n))
           (= (matrix-cols n)
              (matrix-rows m)))
      (calc m n)
      (error "Can't calculate")))
(matrix-*-matrix matrix-list (transpose matrix-list))
(matrix-*-matrix matrix-list matrix-list)
(matrix-*-matrix v (transpose w))
(matrix-*-matrix v w)
