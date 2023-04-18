
(defun print_list_items (l)
  (if (isnull l)
    ()
    (progn
      (print (eval (car l)))
      (if (isnull (cdr l))
        ()
        (progn
          (print " ")
          (print_list_items (cdr l))
        )
      )
    )
  )
)

(defun print_list (l)
  (progn
    (print "(")
    (print_list_items l)
    (println ")")
  )
)

(print_list '(1 2 3 println print_list print_list_items))
