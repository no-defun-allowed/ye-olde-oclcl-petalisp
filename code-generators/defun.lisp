;;; Try to compile (a subset) of any function thrown at oclcl-petalisp.

(in-package #:oclcl-petalisp)

(defvar *already-defined-functions* '(+ - * / = < >
                                      oclcl:get-global-id aref
                                      oclcl:pow oclcl:to-float))

;;; These helpers make for a really consise DSL for translating code.
;;; This isn't concise, but I promise the results are very nice.

(defun stuff-return-in-body (body)
  "Stuff RETURN where it needs to go in a function body BODY."
  (flet ((srib (body) (stuff-return-in-body body)))
    (match body
      ((cons 'progn values)
       `(progn ,@(butlast values) ,(srib (first (last values)))))
      ((list 'if test then else)
       `(if ,test ,(srib then) ,(srib else)))
      ((list* 'let bindings body)
       `(let ,bindings ,@(butlast body) ,(srib (first (last body)))))
      (_ `(return ,body)))))

(defmacro pass-through (binding value &body parts)
  "Probably the most reasonable (and concise) threading macro around.
It's pretty simple too.

input form: (pass-through x y (+ x 2) (* x 3))
expands to: (* (+ y 2) 3)"
  (reduce (lambda (a b)
            (subst a binding b))
          parts
          :initial-value value))

(defun substitute-context (context old-fname body)
  (match body
    ((list* 'let bindings body)
     `(let ,bindings . ,(loop for expr in body collect (substitute-context context old-fname expr))))
    ((cons name args)
     (let ((args (loop for arg in args collect (substitute-context context old-fname arg))))
       (if (eql name old-fname)
           (funcall context (cons name args))
           (loop for part in body collect (substitute-context context old-fname part)))))
    (_ body)))

(defun substitute-name (new old body)
  (substitute-context (lambda (body)
                        (cons new (cdr body)))
                      old body))

(defun funcall-if (function predicate body)
  "Pass any expressions in BODY that satisfy PREDICATE through FUNCTION"
  (cond
    ((funcall predicate body)
     (funcall function body))
    ((consp body)
     (cons (funcall-if function predicate (car body))
           (funcall-if function predicate (cdr body))))
    (t body)))


(defun remove-block (value)
  (match value
    ((list* 'block _ body)
     `(progn . ,(mapcar #'remove-block body)))
    ((cons function arguments)
     (cons function (mapcar #'remove-block arguments)))
    (_ value)))

;;; Here is the actual translator.

(defun resolve-functions (program body)
  "Find every unbound function in BODY and try to compile them into PROGRAM."
  (loop for function in (identify-unbound-functions program body)
        do (let ((code (function-lambda-expression (symbol-function function))))
             (if (null code)
                 (error "can't find definition for ~s" function)
                 (destructuring-bind (λ args &body body) code
                   (assert (eql λ 'lambda))
                   (generate-defun program function args body))))))


(defun generate-defun (program function-name arguments body)
  "Translate a function from a subset of CL to OCLCL"
  (let ((body (loop for expr in body collect
                   (pass-through body expr
                     ; Remove BLOCKs
                     (funcall-if (lambda (x) (and (consp x) (eql (car x) 'block)))
                                 (lambda (x) (third x))
                                 body)
                     ; Add explicit RETURN expressions
                     (stuff-return-in-body body)
                     ; Coerce all integers to floats
                     (funcall-if #'float #'integerp body)
                     ; Rename EXPT and SETF
                     (substitute-name 'oclcl:pow 'expt body)
                     (substitute-name 'set       'setf body)))))
    (oclcl:program-define-function program function-name
                                   'oclcl:float
                                   (loop for arg in arguments
                                         collect (list arg 'oclcl:float))
                                   body)
    (resolve-functions program body)))

(defun identify-unbound-functions (program body)
  "Find unbound functions in BODY that are not found (yet) in PROGRAM"
  (macrolet ((recurse (&rest values)
               `(append . ,(loop for value in values
                              collect `(identify-unbound-functions program ,value))))
             (recurse-list (list)
               (let ((value!g (gensym "VALUE")))
                 `(loop for ,value!g in ,list
                        appending (identify-unbound-functions program ,value!g)))))
    (match body
      ((list 'if test then else)
       (recurse test then else))
      ((list* 'let bindings body)
       (append
        (recurse-list (mapcar #'second bindings))
        (recurse-list body)))
      ((cons 'progn body)
       (recurse-list body))
      ((list 'set _ value)
       (recurse value))
      ((list 'setf _ value)
       (recurse value))
      ((list* 'block _ body)
       (recurse-list body))
      ((list 'return value)
       (recurse value))
      ((cons function args)
       (typecase function
         (symbol
          (append
           (unless (or (member function (oclcl:program-fbound-names program))
                       (member function *already-defined-functions*))
             (list function))
           (recurse-list args)))
         (list
          ;; This is the inner part of a PROGN we fell into somehow.
          (recurse-list body))))
      (_ '()))))