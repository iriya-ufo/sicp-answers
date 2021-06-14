;; 右向き横軸を x 下向き縦軸を y として要素を表現する
;; 任意の要素 (x, y) は (x, y-1) と (x-1, y-1) の要素の和である
;; 条件として (x, y) は自然数かつ x <= y である
;; 1
;; 1 1
;; 1 2 1
;; 1 3 3 1
;; 1 4 6 4 1
;; 1 5 10 10 5 1

(define (pascal-triangle x y)
  (cond ((or (<= x 0) (<= y 0)) 0)
        ((> x y) 0)
        ((or (= x y) (= x 1)) 1)
        ((+ (pascal-triangle x (- y 1))
            (pascal-triangle (- x 1) (- y 1))))))

(pascal-triangle 0 0)
(pascal-triangle 10 1)
(pascal-triangle 3 3)
(pascal-triangle 3 5)
(pascal-triangle 4 6)

(define (show-pascal-triangle line)
  (define (show-pascal-triangle-inter line counter)
    (if (<= line counter)
        (newline)
        (show-pascal-triangle-inter line (+ counter 1))))
  (show-pascal-triangle-inter line 0))
