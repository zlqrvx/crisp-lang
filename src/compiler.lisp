(in-package :crisp)

(defparameter +comment-symbol-char+ '(#\#))
(defparameter +infix-symbol-chars+ '(#\- #\+ #\* #\/ #\= #\! #\@ #\< #\> #\$))
(defparameter +grammer-chars+ '(#\, #\{ #\} #\( #\) #\[ #\] #\| #\;))
(defparameter +whitespace-chars+ '(#\Newline #\Space #\Tab))
(defparameter +keywords+ '("let" "defun" "if" "else" "elif"))


(defun .error (message &rest args)
  (lambda (input)
    (declare (ignore input))
    (apply #'error message args)))

(defun .whitespace ()
  (.first (.map nil (.is 'member +whitespace-chars+))))

(defun .symbol-constituent ()
  (.is-not 'member
	   (concatenate 'list
			(list #\:)
			+comment-symbol-char+
			+infix-symbol-chars+
			+grammer-chars+
			+whitespace-chars+)))

(defun crisp->lisp (crisp-symbol)
   (concatenate 'string
		(loop for c across crisp-symbol
		      if (equal c #\_)
			collect #\-
		      else
			collect (char-upcase c))))

;; (defun .symbol ()
;;   (.let* ((str (.first (.map 'string (.symbol-constituent)))))
;;     ;; If you have passed a keyword something has gone wrong. (may want to remove this evenutally)
;;     (if (member str +keywords+ :test 'string-equal)
;; 	(.fail)
;; 	(.identity (intern (crisp->lisp str) 'crisp-user)))))

(defun .symbol ()
  (.let* ((str-fst (.first (.map 'string (.symbol-constituent))))
	  (str-snd (.optional (.and (.char= #\:)
				    (.first (.map 'string (.symbol-constituent)))))))
    (if (and str-snd (member str-fst +keywords+ :test 'string-equal))
	(.fail)
	(.identity
	 (if str-snd
	     (intern (crisp->lisp str-snd)
		     (intern (crisp->lisp str-fst)))
	     (intern (crisp->lisp str-fst) :crisp-user))))))

;; (caar (run (.symbol) "cl_user:foobar"))
;; (caar (run (.symbol) "foobar"))
;; (caar (run (.symbol) ":foobar"))

(defun .quoted-symbol ()
  (.let* ((_ (.char= #\'))
	  (sym (.symbol)))
	 (.identity `(quote ,sym))))
;; (caar (run (.quoted-symbol) "'foobar"))

(defun .string ()
  (.let* ((_ (.char= #\"))
	  (str (.first (.map 'string (.is-not 'char= #\"))))
	  (_ (.char= #\")))
	 (.identity str)))
;; (caar (run (.string) "\"839\""))

;; (defstruct comment)

;; (defun remove-comments (tree)
;;   (loop for el in tree
;; 	if (equal (type-of el) 'list)
;; 	  collect (remove-comments el)
;; 	else if (not (equal (type-of el) 'comment))
;; 	       collect el))

;; (defun .comment ()
;;   (.let* ((_ (.char= #\#))
;; 	  (_ (.first (.map 'string (.is-not 'char= #\Newline)))))
;;     (.identity (make-comment))))

(defun .number ()
  (.let* ((neg (.optional (.string= "-")))
	  (num-fst (.first (.map 'string (.is #'digit-char-p))))
	  (num-snd (.optional (.and (.char= #\.)
				    (.or (.first (.map 'string (.is #'digit-char-p)))
					 (.error "Number has no decimals."))))))
	 (with-input-from-string (inpt (concatenate 'string neg num-fst "." num-snd))
	   (.identity (read inpt)))))
;; (caar (run (.number) "8.39"))
;; (caar (run (.number) "-8.39"))

(defun .atom ()
  (.or (.number)
       (.string)
       (.quoted-symbol)
       (.symbol)))


(defun .container-constituent ()
  (.let* ((arg (.expr))
	  ;; (arg (.or (.funcall) (.infix) (.atom))) 
	  ;; (arg (.or (.number) (.string) (.symbol)))
	  ;; (_ (.optional (.whitespace)))
	  (_ (.optional (.char= #\,)))
	  (_ (.optional (.whitespace))))
	 (.identity arg)))

(defun .tuple ()
  (.let* ((_ (.char= #\())
	  (args (.first (.map 'list
			      (.container-constituent)
			      :at-least 0)))
	  (_ (.char= #\))))
	 (.identity args)))
;; (caar (run (.tuple) "(a,b, c,sin(d), e + f)"))


(defun .funcall-arg ()
  (.let* ((expr (.expr))
	  (_ (.optional (.whitespace))))
    (.identity (list expr))))
;; (caar (run (.funcall-arg) "4+3"))

(defun .funcall-kwarg ()
  (.let* ((name (.symbol))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\=))
	  (_ (.optional (.whitespace)))
	  (value (.expr)))
    (.identity
     (list
      (intern (string-upcase name) 'keyword)
      value))))
;; (caar (run (.funcall-kwarg) "bob=4+3"))

(defun .funcall-arglist-constituent ()
  (.let* ((arg (.or (.funcall-kwarg) (.funcall-arg)))
	  (_ (.optional (.whitespace)))
	  (_ (.optional (.char= #\,)))
	  (_ (.optional (.whitespace))))
	 (.identity arg)))
;; (caar (run (.funcall-arglist-constituent) "8*3"))
;; (caar (run (.funcall-arglist-constituent) "var=8*3"))

(defun .funcall-arglist ()
  (.let* ((_ (.char= #\())
	  (args (.first (.map 'list
			      (.funcall-arglist-constituent)
			      :at-least 0)))
	  (_ (.char= #\))))
    (.identity (apply 'concatenate 'list args))))
;; (caar (run (.funcall-arglist) "(a,b, c=\"green\", 5*3)"))

(defun .list ()
  (.let* ((_ (.char= #\[))
	  (_ (.optional (.whitespace)))
	  (args (.first (.map 'list
			      (.container-constituent)
			      :at-least 0)))
	  (_ (.char= #\])))
    (.identity (cons 'list args))))
;; (caar (run (.list) "[ a,b, c,sin(d), e + f]"))

(defun .array ()
  (.let* ((_ (.string= "[|"))
	  (_ (.optional (.whitespace)))
	  (args (.first (.map 'list
			      (.container-constituent)
			      :at-least 0)))
	  (_ (.string= "|]")))
    ;; (.identity (magicl:from-list args (list (length args))))))
    (.identity (make-array (length args) :initial-contents args))))
    ;; (.identity args)))
;; (caar (run (.array) "[| 1,8,7,6 |]"))

(defun .matlab-array-constituent ()
  (.let* ((_ (.optional (.whitespace)))
	  (arg (.or (.number) (.char= #\;)))
	  (_ (.optional (.whitespace))))
	 (.identity arg)))

(defun shape-matlab-array (arr)
  (let* ((nrow (1+ (count #\; arr)))
	 (data (delete #\; arr))
	 (size (length data))
	 (ncol (/ size nrow))
	 (md-data
	   (loop for i below nrow
		 collect
		 (loop for j below ncol
		       collect
		       (nth (+ j (* ncol i)) data)))))
    (if (/= (mod size nrow) 0) (error "Incorrect matlab array shape"))
    ;; (magicl:from-list data (list nrow ncol))))
    (make-array (list nrow ncol) :initial-contents md-data)))
    ;; data))

(defun .matlab-array ()
  (.let* ((_ (.string= "[|"))
	  (_ (.optional (.whitespace)))
	  (args (.first (.map 'list
			      (.matlab-array-constituent)
			      :at-least 0)))
	  (_ (.string= "|]")))
    (.identity (shape-matlab-array args))))
;; (caar (run (.matlab-array) "[|1 3 ; 4 5; 6 7|]"))

(defun .paren ()
  (.let* ((_ (.char= #\())
	  (args (.expr))
	  (_ (.char= #\))))
	 (.identity args)))
;; (caar (run (.paren) "(e + f)"))



(defun .funcall ()
  (.let* ((_ (.optional (.whitespace)))
	  (fname (.symbol))
	  (args (.funcall-arglist))
	  (_ (.optional (.whitespace)))
	  )
    (.identity `(,fname ,@args))))
;; (caar (run (.funcall) "+(3 + 2)"))
;; (caar (run (.funcall) "myfun(3, _another_fun(4, 5 - 1, myarg=4))"))
;; (caar (run (.crisp) "ggplot(aes(x=3, y=8), geom_line(colour=\"green\"))"))
;; (caar (run (.crisp) "ggplot(aes(x=[1,2,3], y=[1,4,9]), geom_line(colour=\"green\"))"))


;; TODO: generic indexing
(defun .index ()
  (.let* ((_ (.optional (.whitespace)))
	  (obname (.symbol))
	  (args (.list))
	  (_ (.optional (.whitespace)))
	  )
    (.identity `(,(intern "NTH") ,(cadr args) ,obname))))
;; (caar (run (.index) "my_list[3]"))


(defun .defun-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "defun"))
	  (_ (.whitespace))
	  (name (.symbol))
	  (args (.tuple))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\{))
	  (_ (.optional (.whitespace)))
	  (doc (.optional (.string)))
	  (_ (.optional (.whitespace)))
	  (body (.map 'list (.or (.statement) (.expr)) :at-least 1))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\}))
	  (_ (.optional (.whitespace)))
	  )
    (.identity
     (if doc
	 `(,(intern "DEFUN") ,name ,args ,doc ,@body)
	 `(,(intern "DEFUN") ,name ,args ,@body)))))


(defun .let-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "let"))
	  (_ (.whitespace))
	  (name (.symbol))
	  (_ (.optional (.whitespace)))
	  (_ (.string= "="))
	  (_ (.optional (.whitespace)))
	  (val (.expr))
	  (_ (.optional (.whitespace)))
	  (body (.map 'list (.or (.statement) (.expr)) :at-least 1))
	  (_ (.optional (.whitespace)))
	  )
	 (.identity `(,(intern "LET") ((,name ,val)) ,@body))))


(defun .set-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "set"))
	  (_ (.whitespace))
	  (name (.symbol))
	  (_ (.optional (.whitespace)))
	  (_ (.string= "="))
	  (_ (.optional (.whitespace)))
	  (val (.expr))
	  (_ (.optional (.whitespace)))
	  )
	 (.identity `(,(intern "SETF") ,name ,val))))


(defun .elif-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "elif"))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\())
	  (condition (.expr))
	  (_ (.char= #\)))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\{))
	  (_ (.optional (.whitespace)))
	  (body (.map 'list (.or (.statement) (.expr)) :at-least 1))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\}))
	  (next (.optional (.or (.elif-statement) (.else-statement))))
	  (_ (.optional (.whitespace)))
	  )
    (.identity `(,(intern "IF") ,condition (progn ,@body) ,next))))
    ;; (.identity `(progn ,@body))))

(defun .else-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "else"))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\{))
	  (_ (.optional (.whitespace)))
	  (body (.map 'list (.or (.statement) (.expr)) :at-least 1))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\}))
	  (_ (.optional (.whitespace)))
	  )
    (.identity `(progn ,@body))))

(defun .if-statement ()
  (.let* ((_ (.optional (.whitespace)))
	  (_ (.string= "if"))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\())
	  (condition (.expr))
	  (_ (.char= #\)))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\{))
	  (_ (.optional (.whitespace)))
	  (ybody (.map 'list (.or (.statement) (.expr)) :at-least 1))
	  (_ (.optional (.whitespace)))
	  (_ (.char= #\}))
	  (else (.optional (.or (.elif-statement) (.else-statement))))
	  (_ (.optional (.whitespace)))
	  )
    (.identity `(,(intern "IF") ,condition (progn ,@ybody) ,else))))


(defun infix-op-p (str)
  (member str
	  '("+" "-" "*" "/" "=" "==" "!" "!=" "@" "<" ">" "<=" ">=" "$")
	  :test #'string-equal))

(defstruct op-precedence
  (op nil :type symbol)
  (precedence 0 :type integer))


(defun infix-op-to-precedence (op &key prec)
  (let ((intern-package :crisp-user))
    (trivia:match op
      ("@" (make-op-precedence :op (intern "@" intern-package) :precedence (if prec prec 7)))
      ("*" (make-op-precedence :op (intern "*" intern-package) :precedence (if prec prec 7)))
      ("/" (make-op-precedence :op (intern "/" intern-package) :precedence (if prec prec 7)))
      ("+" (make-op-precedence :op (intern "+" intern-package) :precedence (if prec prec 6)))
      ("-" (make-op-precedence :op (intern "-" intern-package) :precedence (if prec prec 6)))
      ("<" (make-op-precedence :op (intern "<" intern-package) :precedence (if prec prec 4)))
      (">" (make-op-precedence :op (intern ">" intern-package) :precedence (if prec prec 4)))
      ("<=" (make-op-precedence :op (intern "<=" intern-package) :precedence (if prec prec 4)))
      (">=" (make-op-precedence :op (intern ">=" intern-package) :precedence (if prec prec 4)))
      ("==" (make-op-precedence :op (intern "EQUAL" intern-package) :precedence (if prec prec 4)))
      ("!=" (make-op-precedence :op (intern "NOT-EQUAL" intern-package) :precedence (if prec prec 4)))
      ("$" (make-op-precedence :op (intern "RIGHT-ASSOCIATIVE-APPLY" intern-package) :precedence (if prec prec 0)))
      ("=" (make-op-precedence :op (intern "SETQ" intern-package) :precedence (if prec prec 0)))
      (otherwise (make-op-precedence :op (intern op intern-package) :precedence (if prec prec 0))))))


(defparameter *infix-constituent* (list (.funcall) (.index)
					(.atom) (.paren)
					(.list) (.array)
					(.matlab-array)))

(defun .infix-tail ()
  (.let* ((_ (.optional (.whitespace)))
	  (f (.first (.map 'string (.is #'infix-op-p))))
	  (_ (.optional (.whitespace)))
	  ;; (b (.or (.funcall) (.index) (.atom) (.paren)))
	  (b (apply '.or *infix-constituent*))
	  (_ (.optional (.whitespace)))
	  )
    ;; (.identity `(,(intern (infix-op-to-func f)) ,b))
    (.identity `(,(infix-op-to-precedence f) ,b))
    ))
;; (caar (run (.infix-tail) "- 2"))

(defun .infix-full ()
  (.let* ((_ (.optional (.whitespace)))
	  ;; (a (.or (.funcall) (.index) (.atom) (.paren) (.matlab-array)))
	  (a (apply '.or *infix-constituent*))
	  (rst (.infix-tail))
	  )
    (.identity (thread a rst))
    ))
;; (caar (run (.infix-full) "8 - 2"))

(defun thread (list1 list2)
  (cons (car list2) (cons list1 (cdr list2))))


(defun fixpoint (func arg)
  (let* ((current (funcall func arg))
	 (next (funcall func current)))
    (loop while (not (equal current next))
	  do (progn
	       (setq current next)
	       (setq next (funcall func next))))
    next))

(defun sort-infix-tree (tree)
  (trivia:match tree
    ((trivia:guard (list op l r)
		   (equal 'op-precedence (type-of op)))
     (trivia:match l
       ((trivia:guard (list opl ll lr)
		      (equal 'op-precedence (type-of opl)))
	(if (> (op-precedence-precedence op) (op-precedence-precedence opl))
	    (sort-infix-tree
	     (list opl
		   (sort-infix-tree ll)
		   (list
		    op
		    (sort-infix-tree lr)
		    (sort-infix-tree r))))
	    (list op (sort-infix-tree l) (sort-infix-tree r))))
       (otherwise (list op (sort-infix-tree l) (sort-infix-tree r)))))
    (otherwise tree)))
;; (fixpoint 'sort-infix-tree (caar (run (.infix) "a = 8 - 2 * 9 + 3")))

(defun precedence-tree->lisp (tree)
  (trivia:match tree
    ((trivia:guard (list op a b)
		   (equal 'op-precedence (type-of op)))
     (list (op-precedence-op op)
	   (precedence-tree->lisp a)
	   (precedence-tree->lisp b)))
    (x x)))

(defun .infix ()
  (.let* ((op-fst (.infix-full))
	  (op-rst
	   (.optional (.map 'list (.infix-tail)))))
    (.identity
     (precedence-tree->lisp (fixpoint 'sort-infix-tree (reduce 'thread (cons op-fst op-rst)))))
    ))
;; (caar (run (.infix) "myfun(x,y) @ another_fun(a,b)"))
;; (caar (run (.infix) "[|1 2; 3 4|] @ [1,2,3]"))
;; (caar (run (.infix) "print $ 4*2"))

(defun .statement ()
  ;; (.defun-statement)
  (.or (.defun-statement) (.let-statement) (.set-statement) (.if-statement))
  )


(defun multi-plus (args)
  (cond
    ((> (length args) 2)
     (.plus (car args) (.plus (cadr args) (multi-plus (cddr args)))))
    ((= (length args) 2)
     (.plus (car args) (cadr args)))
    ((= (length args) 1) (car args))
    (t args)))

(defun .expr ()
       (multi-plus (list (.infix)
			 (.funcall)
			 (.index)
			 (.atom)
			 (.array)
			 (.matlab-array)
			 (.list))))
;; (defun .expr ()
;;   (.or (.comment) (.plus (.plus (.plus (.funcall) (.index)) (.infix)) (.atom))))
  ;; (.or (.infix) (.funcall) (.atom)))

;; (caar (run (.expr) "sin(3,4+3) + x"))
;; (caar (run (.crisp) "2 * mylist[3] + x"))
;; (caar (run (.crisp) "myfun(x,y) @ another_fun(a,b)"))

(defun .crisp ()
  (.let* ((pr (.map 'list
		    (.let* ((_ (.optional (.whitespace)))
			    (x (.plus (.statement) (.expr)))
			    (_ (.optional (.whitespace)))
			    )
		      (.identity x)))))
    ;; (.identity `(progn ,@(remove-comments pr)))))
    (.identity `(progn ,@pr))))



(defun .extract-comments ()
  (.first
   (.map 'string
	 (.or
	  (.let* ((_ (.char= #\#))
		  (_ (.first (.map 'string (.is-not 'char= #\Newline))))
		  (_ (.char= #\Newline)))
	    (.identity #\Newline))
	  (.item)))))

;; (caar (run (.extract-comments) "
;; print(3)
;; # this is a comment
;; sin(3,4+3) + x # another comment
;; # this is another comment
;; 8^2
;; "))



(defun crisp-compile (str)
  (let ((comment-free (caar (run (.extract-comments) str))))
    (caar (run (.crisp) comment-free))))
