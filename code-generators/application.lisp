;;; Generate programs for petalisp:α

(in-package #:oclcl-petalisp)

(defun generate-α-program (function-name input-count)
  (when (functionp function-name)
    (setf function-name (nth-value 2 (function-lambda-expression function-name))))
  (let* ((kernel-name (gentemp "JIT-KERNEL"))
         (variables   (loop for x below input-count
                            collect (gentemp (format nil "ARG~a" x))))
         (output (gentemp "OUTPUT"))
         (program (oclcl:make-program :name "Petalisp α program"))
         (body  `((let ((pos (oclcl:get-global-id 0)))
                    (set (aref ,output pos)
                         (,function-name . ,(loop for var in variables
                                               collect `(aref ,var pos))))))))
    (oclcl:program-define-function
     program kernel-name 'oclcl:void
     (cons (list output 'oclcl:float*)
           (loop for var in variables
                 collect (list var 'oclcl:float*)))
     body)
    (resolve-functions program body)
    (values program kernel-name)))
                                 
