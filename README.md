# toylisp
a toy lisp interpreter in C

example:
```
(defun fib (n)
  (progn
    (cond
      ((< n 3) 1)
      (t (+ (fib (- n 1)) (fib (- n 2)))))))

(for (set i 1) (<= i 10) (++ i)
  (print (fib i)))

; output: 1 1 2 3 5 8 13 21 34 55 NIL
```
