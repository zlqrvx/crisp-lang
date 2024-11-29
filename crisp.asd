(defclass crisp-file (source-file)
  ())
(defmethod perform ((o load-source-op) (c crisp-file))
    nil)
(defmethod perform ((o load-op) (c crisp-file))
    nil)
(defmethod perform ((o compile-op) (c crisp-file))
    nil)


(asdf:defsystem "crisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("smug" "trivia" "alexandria" "lla" "cl-num-utils")
  ;; Build an executable with: (asdf:make :crisp)
  :build-operation "program-op" ;; leave as is
  :build-pathname "crisp"
  :entry-point "crisp:repl"
  :components ((:module "src"
                :components
                ((:file "package")
		 (:file "compiler")
		 (:file "repl")
		 (:file "builtin")))
	       (:module "src/stdlib"
		:components
		((:crisp-file "misc.crisp")
		 (:file "stdlib"
			:depends-on ("misc.crisp")))))
  :description ""
  :in-order-to ((test-op (test-op "crisp/tests"))))


(asdf:defsystem "crisp/tests"
  :author ""
  :license ""
  :depends-on ("crisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for crisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
