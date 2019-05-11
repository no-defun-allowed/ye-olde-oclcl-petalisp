(in-package #:oclcl-petalisp)

(defun range-size (range)
  (check-type range range)
  (/ (1+ (- (range-end range) (range-start range)))
     (range-step range)))

(defun shape-size (shape)
  (check-type shape shape)
  (loop with size = 1
        for range in (ranges shape)
        do (setf size (* size (range-size range)))
        finally (return (the integer size))))

(defclass gpu-array (petalisp.core:non-empty-immediate)
  ((backing-vector :initarg :backing-vector :reader gpu-array-backing-vector)
   (petalisp.core::%type-code
    :initform (petalisp.type-codes:type-code-from-type-specifier 'single-float))
   (shape :initarg :shape :reader gpu-array-shape :reader shape)))

(defun make-gpu-array (shape)
  (make-instance 'gpu-array
                 :backing-vector (cffi:foreign-alloc :float :count (shape-size shape))
                 :shape shape))

(defmethod petalisp.core:lisp-datum-from-immediate ((gpu-array gpu-array))
  (let ((cl-array (make-array (mapcar #'range-size (ranges (gpu-array-shape gpu-array)))
                              :element-type 'single-float))
        (c-vector (gpu-array-backing-vector gpu-array))
        (size (shape-size (gpu-array-shape gpu-array))))
    (dotimes (x size)
      (setf (row-major-aref cl-array x)
            (cffi:mem-aref c-vector :float x)))
    cl-array))
    
(defmethod petalisp.core:overwrite-instance ((instance lazy-array) (replacement gpu-array))
  (change-class instance 'gpu-array
                :backing-vector (gpu-array-backing-vector replacement)
                :shape (gpu-array-shape replacement)))
