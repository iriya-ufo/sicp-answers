(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (iota board-size 1)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position row col rest)
  (cons (list row col) rest))

(define (check a b)
  (let ((ax (car a))
        (ay (cadr a))
        (bx (car b))
        (by (cadr b)))
    (and (not (= ax bx))
         (not (= ay by))
         (not (= (abs (- ax bx)) (abs (- ay by)))))))

(define (safe? y)
  (= 0 (accumulate + 0
                   (map (lambda (x)
                          (if (check (car y) x) 0 1))
                        (cdr y)))))

(define empty-board '())

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(queens 8)
(length (queens 8))
(length (queens 5))
