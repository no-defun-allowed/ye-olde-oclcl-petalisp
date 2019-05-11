(in-package #:oclcl-petalisp)

(defun run-α-program (backend program inputs output-shape)
  (dolist (input inputs)
    (check-type input gpu-array))
  (let* ((queue (create-command-queue-with-properties (context backend) (first (devices backend))))
         (float-size (cffi:foreign-type-size :float))
         (buffers (loop for input in inputs
                        collect (create-buffer (context backend)
                                               :mem-read-only
                                               (* (shape-size (shape input)) float-size))))
         (output-buffer (create-buffer (context backend) :mem-write-only
                                       (* float-size (shape-size output-shape)))))
    (loop for input in inputs
          for buffer in buffers
          do (%ocl:enqueue-write-buffer queue buffer %ocl:true
                                        0 (* (shape-size (shape input)) float-size)
                                        (gpu-array-backing-vector input)
                                        0 (cffi:null-pointer) (cffi:null-pointer)))
    (cffi:with-foreign-array (work-size '%ocl:size-t (list (shape-size output-shape)))
      (let ((kernel (create-kernel (cached-α-program-program program)
                                   (cached-α-program-entry-point program))))
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
