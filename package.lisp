(defpackage :oclcl-petalisp
  (:use :cl :eazy-opencl.host :petalisp :trivia)
  (:export #:record-defun #:oclcl-backend #:make-oclcl-backend #:list-platforms #:choose-device))
