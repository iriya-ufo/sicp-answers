;; ex1.41.scm

;; double�ϰ����Ȥ��ư�����μ�³�����ꡢ
;; ������ä���³���������Ѥ������³�����֤�

(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))

(print ((double inc) 2))
(print ((double square) 2))

(print (((double (double double)) inc) 5))
