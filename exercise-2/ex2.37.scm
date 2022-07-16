(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define v '(1 1 0 1))
(define w '(1 0 1 1))
(define n '((0 1 1 1) (1 0 1 1) (1 1 0 1) (1 1 1 0)))

;; m
;; +-         -+
;; |  1 2 3 4  |
;; |  4 5 6 6  |
;; |  6 7 8 9  |
;; +-         -+

;; v
;; +-         -+
;; |  1 1 0 1  |
;; +-         -+

;; w
;; +-         -+
;; |  1 0 1 1  |
;; +-         -+

;; n
;; +-         -+
;; |  0 1 1 1  |
;; |  1 0 1 1  |
;; |  1 1 0 1  |
;; |  1 1 1 0  |
;; +-         -+

;; m * n
;; +-            -+
;; |  9  8  7  6  |
;; |  17 16 15 15 |
;; |  24 23 22 21 |
;; +-            -+

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
(dot-product v w)                       ; => 2

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))
(matrix-*-vector m v)                   ; => (7 15 22)

(define (transpose mat)
  (accumulate-n cons '() mat))
(transpose m)                           ; => ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (u) (matrix-*-vector cols u)) m)))
(matrix-*-matrix m n)                   ; => ((9 8 7 6) (17 16 15 15) (24 23 22 21))
