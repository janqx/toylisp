; the library of toylisp

(set list 
  (lambda (&rest args) args))

(defmacro defun (name params body) 
  (list 'set name (list 'lambda params body)))

(defun instanceof (a b) 
  (== (typeof a) (typeof b)))

(defun typeif (a b) 
  (== (typeof a) b))

(defmacro if (test then else) 
  (list 'cond (list test then) (list T else)))

(defmacro and (a b) 
  (list 'if a (list 'if b T NIL) NIL))

(defmacro or (a b)
  (list 'if a T (list 'if b T NIL)))

(defmacro isnull (a) (list '== a 'NIL))

(defmacro atom (x) 
  (list '!= (list 'typeof x) ''CONS))

(defun length (l)
  (if (== l NIL)
    0
    (+ 1 (length (cdr l)))))

(defmacro ++ (i)
  (list 'progn (list 'set i (list '+ i 1)) i)
)

(defmacro -- (i)
  (list 'progn (list 'set i (list '- i 1)) i)
)

(defmacro swap (a b)
  (list 'progn (list 'set '__temp a) (list 'set a b) (list 'set b '__temp))
)

(defmacro for (_init _cond _iter _body)
  (list 'progn _init (list 'while _cond (list 'progn _body _iter)))
)
