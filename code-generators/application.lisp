;;; Generate programs for petalisp:α

(in-package #:oclcl-petalisp)

(defun generate-α-program (function-name inputs)
  (when (functionp function-name)
    (setf function-name (nth-value 2 (function-lambda-expression function-name))))
  (let* ((kernel-name (gentemp "APPLICATION-KERNEL"))
         (variables   (loop for nil in inputs
			    for x from 0
                            collect (gensym (format nil "ARG~d" x))))
	 (outputs     (loop for nil in inputs
			    for x from 0
                            collect (gensym (format nil "OUT~d" x))))
         (output (gensym "OUTPUT"))
         (program (oclcl:make-program :name "Petalisp α program"))
         (body  `((let ((pos (oclcl:to-int (oclcl:get-global-id 0))))
		    (let* ,(loop for array-var in variables
			         for output-var in outputs
			         for input in inputs
			         appending (aref-letter input array-var output-var 'pos))
                      (set (aref ,output pos)
                           (,function-name . ,outputs)))))))
    (add-stdlib program)
    (oclcl:program-define-function
     program kernel-name 'oclcl:void
     (cons (list output 'oclcl:float*)
           (loop for var in variables
                 collect (list var 'oclcl:float*)))
     body)
    (resolve-functions program body)
    (values program kernel-name)))
                                 
