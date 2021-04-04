;; ex2.41.scm

(use srfi-1)				; iota

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
	  (accumulate op initial (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; (flatmap
;;  (lambda (i)
;;    (map (lambda (j) (list i j))
;; 	(iota (- i 1) 1)))
;;  (iota 3 1))


;; (define (unique-triples n)
;;   (flatmap
;;    (lambda (i)
;;      (map (lambda (j) (list i j))
;; 	  (iota (- i 1) 1)))
;;    (iota n 1)))

(unique-triples 3)

(define (ordered-triples n s)
