;; ex2.13.scm
;; 未完

;; x の相対許容誤差を p[x] , y の相対許容誤差を p[y] とすると

;; (x ± p[x]x)・(y ± p[y]y) → xy ± (p[x] + p[y] + p[x]p[y])xy

;; ここで, p[x] および p[y] が小さいとすると p[x]p[y] の項は
;; 無視することができるので2つの区間の積は

;; xy ± (p[x] + p[y])xy

;; で近似できる
;; したがって2つの区間の積の相対許容誤差は2つの区間の相対許容誤差の和で近似できる
