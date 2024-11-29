(defpackage crisp
  (:use :cl :smug)
  (:export
   #:repl
   #:crisp-eval
   #:crisp-eval-file))

(defpackage crisp-base
  (:use :cl)
  (:export
   ;; Builtin crisp symbols
   #:library
   #:not-equal
   #:help
   #:right-associative-apply
   #:lisp-symbol
   #:alias-function
   #:c
   ;; Inherited cl symbols
   #:defun
   #:quote
   #:if
   #:let
   #:* #:/ #:+ #:-
   #:print
   #:equal #:< #:> #:<= #:>=
   #:sin #:cos #:tan
   #:funcall #:lambda
   #:declare
   #:optimize #:speed #:space #:safety #:debug #:compilation-speed
   #:type #:fixnum #:integer
   ))

(defpackage crisp-user
  (:use :crisp-base))
