;;; Generate aref for GPU arrays and references

(in-package #:oclcl-petalisp)

(defgeneric aref-letter (input array-symbol output-symbol index))

(defmethod aref-letter ((input gpu-array) array-symbol output-symbol index)
  `((,output-symbol (aref ,array-symbol ,index))))

(defmethod aref-letter ((input petalisp.core:array-immediate) array-symbol output-symbol index)
  `((,output-symbol (aref ,array-symbol ,index))))

(defmethod aref-letter ((reference petalisp.core:reference) array-symbol output-symbol index)
  (let* ((indices (loop for ranges in (ranges (shape reference))
    	   	        collect (gensym "INDEX")))
 	 (index!g (gensym "ORIGINAL-INDEX"))
         (input-sizes (loop for range in (reverse (ranges (shape reference)))
    	 	            collect (multiple-value-bind (start step end)
					(range-start-step-end range)
				      (1+ (/ (- end start) step)))))
	 (transform (transformation reference))
	 (transformed!gs (loop repeat (petalisp.core::output-rank transform)
			       collect (gensym "TRANSFORMED")))
	 (output-sizes (loop for range in (ranges (shape (first (inputs reference))))
			     collect (multiple-value-bind (start step end)
					 (range-start-step-end range)
				       (1+ (/ (- end start) step))))))
    (append
     `((,index!g ,index))
     ;; Expand n indices from single input index
     (loop with acc = 1
	   for size in input-sizes
	   for index in indices
	   collect `(,index (mod (/ ,index!g ,acc) ,size))
  	   do (setf acc (* acc size)))
     ;; Transform indices
     (loop for index   across (petalisp.core::output-mask transform)
	   for scaling across (petalisp.core::scalings transform)
	   for offset  across (petalisp.core::offsets transform)
 	   for input = (if (null index) 0 (elt indices index))
	   for transform-name in transformed!gs
  	   collect `(,transform-name (+ ,offset (* ,input ,scaling))))
     ;; aref the backing values now
     (aref-letter (first (inputs reference)) array-symbol output-symbol
		  ;; Squish the indices to a single input
		  (let ((parts
			 (loop with acc = 1
		               for transform-name in (reverse transformed!gs)
			       for size in output-sizes
		               collect `(* ,acc ,transform-name)
		   	       do (setf acc (* acc size)))))
		    (cond
		      ((null parts) 0)
		      ((null (cdr parts)) (car parts))
		      (t (cons '+ parts))))))))
