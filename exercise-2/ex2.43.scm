;;; Fast Queen Puzzle
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (print "new-row: " new-row ", k: " k ", rest-of-queens: " rest-of-queens ", adj: " (adjoin-position new-row k rest-of-queens))
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

;;; Slow Queen Puzzle
(define (slow-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (print "new-row: " new-row ", k: " k ", rest-of-queens: " rest-of-queens ", adj: " (adjoin-position new-row k rest-of-queens))
                   (adjoin-position
                    new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (iota board-size 1)))))
  (queen-cols board-size))

(time (queens 8))
; real   0.037
; user   0.050
; sys    0.000

(time (slow-queens 8))
; real  29.030
; user  52.300
; sys    2.050

(time (queens 3))
(time (slow-queens 3))

(queens 3)
(slow-queens 3)

;; cf.
;; http://community.schemewiki.org/?sicp-ex-2.43
