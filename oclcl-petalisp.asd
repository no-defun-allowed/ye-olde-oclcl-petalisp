(asdf:defsystem :oclcl-petalisp
  :description "A OpenCL/oclcl backend for Petalisp"
  :author "Hayley Patton (no-defun-allowed)"
  :license "AGPLv3"
  :depends-on (petalisp oclcl eazy-opencl)
  :components ((:file "package")
               (:file "device-picker")
	       (:module backend
                        :components ((:file "gpu-array")
                                     (:file "run-opencl")
                                     (:file "backend")))
               (:module code-generators
                        :components ((:file "defun")
				     (:file "gpu-aref")
				     (:file "stdlib")
				     (:file "reduction")
                                     (:file "application")))))
