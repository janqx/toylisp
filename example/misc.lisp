(for (set i 1) (<= i 9) (++ i)
  (progn
    (for (set j 1) (<= j i) (++ j)
      (progn
        (print j)
        (print "*")
        (print i)
        (print "=")
        (print (* i j))
        (if (>= (* i j) 10)
          (print " ")
          (print "  ")
        )
      )
    )
    (println "")
  )
)


