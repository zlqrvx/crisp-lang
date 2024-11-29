(in-package :crisp-user)



(cl:eval-when (:compile-toplevel)
  (cl:flet ((load-stdlib-file (fname)
	   (crisp:crisp-eval-file
	    (cl:merge-pathnames fname cl:*compile-file-truename*))))

    (load-stdlib-file "misc.crisp")

    ))

