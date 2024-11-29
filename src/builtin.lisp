(in-package :crisp-base)


(defun library (fname)
  "Load the crisp file fname"
  (crisp:crisp-eval-file fname))

(defun not-equal (x y)
  (not (equal x y)))

(defmacro help (symbol)
  "Lookup help on symbol"
  (describe symbol)
  (values))

(defmacro right-associative-apply (f args)
  `(funcall (quote ,f) ,args))


(defun lisp-symbol (symbol package)
  (intern (string-upcase symbol)
	  (intern (string-upcase package))))


(defmacro alias-function (to from)
  `(progn
     (defvar ,to)
     (setf (fdefinition (quote ,to))
	   (symbol-function ,from))))

(defun c (&rest contents)
  (make-array (length contents)
	      :initial-contents contents))

