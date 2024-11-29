(in-package :crisp)

(defvar *debug-mode* nil)

(defun crisp-eval (code)
  (let ((lisp-code (crisp-compile code)))
    (if *debug-mode*
	(progn
	  (format t "~A" lisp-code) (terpri)))
    (handler-case
	(eval lisp-code)
      (warning (c)
	(format t "❗WARNING❗ ~a~%" c))
      (t (c)
	(format t "‼️ERROR‼️ ~a~%" c)))))

(defun crisp-eval-file (fname)
  (let ((code
	  (alexandria:read-file-into-string fname)))
	(crisp-eval code)))


(defun repl-prompt (stdout stdin)
  (princ "> " stdout)
  (force-output)
  (read-line stdin))


(defun repl ()
  (let ((stdout *standard-output*)
	(stdin *standard-input*))
    (loop
      with code = (repl-prompt stdout stdin)
      do (progn
	   (trivia:match code
	     ("" nil)
	     (":q" (return))
	     (":debug" (progn
			 (setq *debug-mode* (not *debug-mode*))
			 (if *debug-mode*
			     (princ "Debug mode: ON" stdout)
			     (princ "Debug mode: OFF" stdout))
			 (terpri stdout)))
	     (otherwise
	      (cond
		((uiop:string-prefix-p ":i " code)
		 (describe (crisp-eval (subseq code 3)) stdout)
		 (terpri stdout))
		((uiop:string-prefix-p ":d " code)
		 (disassemble (crisp-eval (subseq code 3)) :stream stdout)
		 (terpri stdout))
		(t (progn
		     (princ (crisp-eval code) stdout)
		     (terpri stdout))))))
	   ;; Now get more input
	   (setq code (repl-prompt stdout stdin)))))
  t)

