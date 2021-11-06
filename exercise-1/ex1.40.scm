(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
             (print next))
            (else (print guess)
                  (try next)))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

;; 例
;; x = 0,2,-2 を解とする3次方程式
;; y = x(x-2)(x+2) = x^3 - 4x

(newtons-method (cubic 0 -4 0) 0.3)
;; => -0.014477970599550083
;; => -5.688101526520484e-17

(newtons-method (cubic 0 -4 0) 5.0)
;; => 3.5211298848700348
;; => 2.630286464754836
;; => 2.1721480649563873
;; => 2.0185158881636878
;; => 2.000251827239335
;; => 2.0000000000003726

(newtons-method (cubic 0 -4 0) -5.0)
;; => -3.521123636146351
;; => -2.6302772521440425
;; => -2.172139774321156
;; => -2.018512281709883
;; => -2.000251461269259
;; => -1.99999999999966

;; 例
;; x = -1,3,5 を解とする3次方程式
;; y = (x+1)(x-3)(x-5) = x^3 - 7x^2 + 7x + 15

(newtons-method (cubic -7 7 15) 4.3)
;; => 6.4246143824582695
;; => 5.538591117790794
;; => 5.122542324330658
;; => 5.008840871076889
;; => 5.000051671190534
;; => 5.000000000000014

(newtons-method (cubic -7 7 15) 2.0)
;; => 3.0000000000024696

(newtons-method (cubic -7 7 15) -0.8)
;; => -1.0190865106416056
;; => -1.0001498985826975
;; => -0.9999999999999637

(newtons-method (cubic -7 7 15) 4.0)
;; => -1.0002500100458187
;; => -0.9999999999998961
