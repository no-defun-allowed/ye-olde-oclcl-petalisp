;;; A backend for Petalisp that creates and runs OpenCL code

(in-package #:oclcl-petalisp)

(defclass oclcl-backend (petalisp.core:backend)
  ((platform :initarg :platform :reader platform)
   (α-cached-programs :initform (make-hash-table :test 'equal) :reader α-cache)
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

(defstruct cached-α-program program entry-point)

(declaim (inline range->list shape->list))
(defun range->list (range)
  (multiple-value-list (range-start-step-end range)))
(defun shape->list (shape)
  (mapcar #'range->list (ranges shape)))
(defun hash-shapes (input-shapes output-shape operator)
  (list* operator
         (shape->list output-shape)
         (mapcar #'shape->list input-shapes)))

(defun find-α-program (operator input-count backend)
  (let ((hash (list operator input-count)))
    (multiple-value-bind (program win)
        (gethash hash (α-cache backend))
      (if win
          program
          (multiple-value-bind (oclcl-program entry-point)
              (generate-α-program operator input-count)
            (let* ((source (oclcl:compile-program oclcl-program))
                   (program (create-program-with-source (context backend) source)))
              (build-program program :devices (devices backend))
              (setf (gethash hash (α-cache backend))
                    (make-cached-α-program :program program
                                           :entry-point
                                           (oclcl.lang.util:c-identifier entry-point t)))))))))
                  

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
         (input-shapes (mapcar #'shape inputs))
         (program (find-α-program (operator application) (length (inputs application)) backend)))
    (run-α-program backend program inputs (shape application))))
