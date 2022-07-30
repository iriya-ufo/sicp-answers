(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (iota (- i 1) 1)))
   (iota n 1)))

(unique-triples 3)

(unique-pairs 4)
