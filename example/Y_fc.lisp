; Realizing recursive factorial by Y combinator

(set Y
  (lambda (f) 
    ((lambda (g) (g g))
      (lambda (g) (f (lambda (x) ((g g) x)))))))

(defun fc (g) 
  (lambda (x) 
    (cond ((<= x 0) 1) ((> x 0) (* x (g (- x 1)))))))

(println ((Y fc) 10))
