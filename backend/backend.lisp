;;; A backend for Petalisp that creates and runs OpenCL code

(in-package #:oclcl-petalisp)

(defclass oclcl-backend (petalisp.core:backend)
  ((platform :initarg :platform :reader platform)
   (α-cached-programs :initform '() :accessor α-cache)
   (β-cached-programs :initform '() :accessor β-cache)
   (devices  :initarg :devices  :reader devices)
   (context  :initarg :context  :reader context))
  (:documentation "A backend that can use a GPU (using OpenCL) to run Petalisp
programs. When creating an instance of OCLCL-BACKEND, we expect a :platform,
:devices, and a :context which have the same meanings as in OpenCL.

The utility function CHOOSE-DEVICE (and LIST-PLATFORMS) may be helpful to
instantiate a backend."))

(defun make-oclcl-backend (&key (platform (first (get-platform-ids)))
                                (devices  (get-device-ids platform :device-type-default))
                                (context  (create-context devices)))
  "Instantiate an OpenCL backend. PLATFORM, DEVICES and CONTEXT are chosen
using the first available platform by default."
  (make-instance 'oclcl-backend :platform platform :devices devices :context context))

;;;; Caching programs
;;; This is not very efficient but anything is faster than recompiling (:
(defstruct cached-program program entry-point)
(defun range->list (range)
  (multiple-value-list (range-start-step-end range)))
(defun hash-transform (transform)
  (flet ((listify (x) (coerce x 'list)))
    (append (listify (petalisp.core::input-mask transform))
	    (listify (petalisp.core::output-mask transform))
	    (listify (petalisp.core::offsets transform))
	    (listify (petalisp.core::scalings transform)))))
(defun hash-input (input)
  (etypecase input
    (array-immediate (mapcar #'range->list (ranges (shape input))))
    (gpu-array       (mapcar #'range->list (ranges (gpu-array-shape input))))
    (reference (append (mapcar #'range->list (ranges (shape input)))
		       (hash-transform (transformation input))))))
(defun hash-program (inputs operator)
  (cons operator (mapcar #'hash-input inputs)))

(defun find-α-program (operator inputs backend)
  (let ((hash (hash-program inputs operator)))
    (let ((program-pair (assoc hash (α-cache backend) :test #'tree-equal)))
      (if program-pair
	  (cdr program-pair)
          (multiple-value-bind (oclcl-program entry-point)
              (generate-α-program operator inputs)
	    (let ((*print-array* nil))
	      (format *debug-io* "Compiling a program for~%  (α ~s ~{~s~^ ~})..."
		      operator inputs))
            (let* ((source (oclcl:compile-program oclcl-program))
                   (program (create-program-with-source (context backend) source))
		   (cache (make-cached-program :program program
                                               :entry-point
                                               (oclcl.lang.util:c-identifier entry-point t))))
              (build-program program :devices (devices backend))
	      (format *debug-io* "done~%")
              (push (cons hash cache)
		    (α-cache backend))
	      cache))))))

(defun find-β-program (operator input backend)
  (let ((hash (hash-program (list input) operator)))
    (let ((program-pair (assoc hash (β-cache backend) :test #'tree-equal)))
      (if program-pair
	  (cdr program-pair)
	  (multiple-value-bind (oclcl-program entry-point)
	      (generate-β-program operator input)
	    (let ((*print-array* nil))
	      (format *debug-io* "Compiling a program for~%  (β ~s ~s)..."
		      operator input))
            (let* ((source (oclcl:compile-program oclcl-program))
                   (program (create-program-with-source (context backend) source))
		   (cache (make-cached-program :program program
                                               :entry-point
                                               (oclcl.lang.util:c-identifier entry-point t))))
              (build-program program :devices (devices backend))
	      (format *debug-io* "done~%")
              (push (cons hash cache)
		    (β-cache backend))
	      cache))))))

;;;; Backend operations
(defgeneric evaluate (array backend))
(defmethod petalisp.core:compute-immediates ((arrays list) (backend oclcl-backend))
  (loop for array in arrays collect (evaluate array backend)))

(defmethod evaluate ((array array-immediate) backend)
  (let* ((size (shape-size (shape array)))
         (c-array (cffi:foreign-alloc :float :count size))
         (array-to-copy (storage array))
         (gpu-array (make-instance 'gpu-array :backing-vector c-array :shape (shape array))))
    (dotimes (x size)
      (setf (cffi:mem-aref c-array :float x)
            (coerce (row-major-aref array-to-copy x) 'single-float)))
    gpu-array))

(defmethod evaluate ((application application) backend)
  (let* ((inputs (loop for input in (inputs application) collect (evaluate input backend)))
         (program (find-α-program (operator application) inputs backend)))
    (run-program backend program inputs (shape application))))

(defmethod evaluate ((reduction reduction) backend)
  (assert (null (cdr (inputs reduction))))
  (let* ((inputs (loop for input in (inputs reduction) collect (evaluate input backend)))
         (program (find-β-program (operator reduction) (first inputs) backend)))
    (run-program backend program inputs (shape reduction))))

(defmethod evaluate ((reference reference) backend)
  (make-instance 'reference :transformation (transformation reference)
		            :type-code (petalisp.core:type-code reference)
		            :shape (shape reference)
			    :inputs (list (evaluate (first (inputs reference)) backend))))
