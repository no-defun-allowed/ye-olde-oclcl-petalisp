(in-package #:oclcl-petalisp)

(defun run-program (backend program inputs output-shape
		    &key (sizer (lambda (input) (* (shape-size (shape input))))))
  (setf inputs
	(loop for input in inputs
   	      collect (etypecase input
			(gpu-array input)
			(reference (first (inputs input))))))
  (let* ((queue (create-command-queue (context backend) (first (devices backend))))
         (float-size (cffi:foreign-type-size :float))
         (buffers (loop for input in inputs
                        collect (create-buffer (context backend) :mem-read-only (* (funcall sizer input) float-size))))
         (output-buffer (create-buffer (context backend) :mem-write-only
                                       (* float-size (shape-size output-shape)))))
    (loop for input in inputs
          for buffer in buffers
          do (%ocl:enqueue-write-buffer queue buffer %ocl:true
                                        0 (* (shape-size (shape input)) float-size)
                                        (gpu-array-backing-vector input)
                                        0 (cffi:null-pointer) (cffi:null-pointer)))
    (cffi:with-foreign-array (work-size '%ocl:size-t (list (shape-size output-shape)))
      (let ((kernel (create-kernel (cached-program-program program)
                                   (cached-program-entry-point program))))
        (set-kernel-arg kernel 0 output-buffer '%ocl:mem)
        (loop for buffer in buffers
              for n from 1
              do (set-kernel-arg kernel n buffer '%ocl:mem))
        
        (%ocl:enqueue-nd-range-kernel queue kernel 1 (cffi:null-pointer)
                                      work-size (cffi:null-pointer)
                                      0 (cffi:null-pointer) (cffi:null-pointer))))
    (let ((output-array (make-gpu-array output-shape)))
      (%ocl:enqueue-read-buffer queue output-buffer %ocl:true
                                0 (* (shape-size output-shape) float-size)
                                (gpu-array-backing-vector output-array)
                                0 (cffi:null-pointer) (cffi:null-pointer))
      output-array)))
