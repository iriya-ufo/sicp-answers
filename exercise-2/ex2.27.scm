(define x (list (list 1 2) (list 3 4)))
(define y (list (list 5 6) (list 7 8)))
(define xy (list x y))

x
y
xy

(reverse x)
(reverse y)
(reverse xy)

(define (deep-reverse ls)
  (if (not (pair? ls))
      ls
      (reverse (map deep-reverse ls))))

(reverse (map
          (lambda (z) reverse (map (lambda (ls) reverse (map reverse ls)) z))
          xy))

(reverse (map (reverse (map reverse xy))))
(reverse (map (lambda (ls) reverse (map reverse ls)) xy))
(reverse (map (lambda (ls) (map reverse ls)) (map reverse xy)))

(map (reverse (map reverse xy)) xy)
(reverse (map reverse xy))

(reverse (map (reverse (lambda (ls) map reverse) ls) xy))
(reverse (map (lambda (ls) reverse (map reverse ls)) xy))
(map (lambda (ls) reverse (map reverse ls)) xy)
xy

(deep-reverse y)
(deep-reverse xy)

(define z '((1 2) (3 4 (5 6 7))))
z
(reverse z)
(map reverse z)
(reverse (map reverse z))

(define x '(1 2))
(define xx '(3 4))
(define y '(5 6))
(define yy '(7 8))

(define z (list (list x xx) (list y yy)))
z

x
xx
y
yy

(map (map reverse z))
