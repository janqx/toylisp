(defun fib-recursive (n)
  (if (< n 3)
    1
    (+ (fib-recursive (- n 1)) (fib-recursive (- n 2)))
  )
)

(defun fib-non-recursive (n)
  (progn
    (defun fib-iter (a b n)
      (if (== n 1)
        b
        (fib-iter b (+ a b) (-- n))))
    (if (< n 3)
      1
      (fib-iter 0 1 n)) 
  )
)

(println (fib-non-recursive 20))

(println (fib-recursive 10))
