;;; Generate programs for petalisp:β

(in-package #:oclcl-petalisp)

(defun generate-β-program (function-name input)
  (when (functionp function-name)
    (setf function-name (nth-value 2 (function-lambda-expression function-name))))
  (let* ((kernel-name (gentemp "REDUCTION-KERNEL"))
	 (input-name  (gentemp "INPUT"))
	 (output      (gentemp "OUTPUT"))
	 (program     (oclcl:make-program :name "Petalisp β program"))
	 (size (multiple-value-bind (start step end)
		   (range-start-step-end (first (last (ranges (shape input)))))
		 (1+ (/ (- end start) step))))
	 (body `((let ((pos (oclcl:to-int (oclcl:get-global-id 0))))
		   (let ((acc (aref ,input-name (* pos ,size))))
		     (do ((step 1 (+ step 1)))
			 ((= step ,size))
		       (set acc (,function-name (aref ,input-name (+ step (* pos ,size))) acc)))
		     (set (aref ,output pos) acc))))))
    (add-stdlib program)
    (oclcl:program-define-function
     program kernel-name 'oclcl:void
     `((,output oclcl:float*) (,input-name oclcl:float*))
     body)
    (resolve-functions program body)
    (values program kernel-name)))
