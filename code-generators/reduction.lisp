;;; Generate programs for petalisp:β

(in-package #:oclcl-petalisp)

(defun generate-β-program (function-name input)
  (when (functionp function-name)
    (setf function-name (nth-value 2 (function-lambda-expression function-name))))
  (let ((rewrite-pair (assoc function-name *replace-functions*)))
    (when rewrite-pair
      (setf function-name (second rewrite-pair))))
  (let* ((kernel-name (gentemp "REDUCTION-KERNEL"))
	 (input-name  (gentemp "INPUT"))
	 (output      (gentemp "OUTPUT"))
	 (program     (oclcl:make-program :name "Petalisp β program"))
	 (first-size (multiple-value-bind (start step end)
			 (range-start-step-end (first (ranges (shape input))))
		       (1+ (/ (- end start) step))))
	 (size (/ (shape-size (shape input)) first-size))
	 (body `((let ((pos (oclcl:to-int (oclcl:get-global-id 0))))
		   (let ((acc (aref ,input-name pos)))
		     (do ((step 1 (+ step 1)))
			 ((= step ,first-size))
		       (set acc (,function-name (aref ,input-name (+ (* step ,size) pos)) acc)))
		     (set (aref ,output pos) acc))))))
    (add-stdlib program)
    (oclcl:program-define-function
     program kernel-name 'oclcl:void
     `((,output oclcl:float*) (,input-name oclcl:float*))
     body)
    (resolve-functions program body)
    (values program kernel-name)))
