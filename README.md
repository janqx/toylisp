# toylisp
a toy lisp interpreter in C

example:
```
(defun fib (n)
  (progn
    (cond
      ((< n 3) 1)
      (t (+ (fib (- n 1)) (fib (- n 2)))))))

(println (fib 10))

; output: 55
```
