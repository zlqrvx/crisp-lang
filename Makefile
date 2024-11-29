LISP ?= sbcl


build:
	$(LISP) --eval '(require :asdf)' \
		--eval '(asdf:load-asd (merge-pathnames (uiop:getcwd) "./crisp.asd"))' \
		--eval '(asdf:load-system :crisp)' \
		--eval '(asdf:make :crisp)' \
		--eval '(quit)'


test:
	$(LISP) --eval '(require :asdf)' \
		--eval '(asdf:load-asd (merge-pathnames (uiop:getcwd) "./crisp.asd"))' \
		--eval '(asdf:test-system :crisp)' \
		--eval '(quit)'
