;;; Some standard functions and macros for oclcl code

(in-package #:oclcl-petalisp)

(defun add-stdlib (program)
  (oclcl:program-define-macro program
			      'let* '(bindings &body body)
			      '((if (null bindings)
				    `(progn ,@body)
				    `(let (,(car bindings))
				       (let* ,(cdr bindings)
					 ,@body))))))
