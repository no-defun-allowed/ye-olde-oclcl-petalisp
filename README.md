# petalisp-oclcl (or oclcl-petalisp?)

petalisp-oclcl is a [Petalisp](https://github.com/marcoheisig/Petalisp) backend
that runs Petalisp programs on GPUs using [oclcl](https://github.com/gos-k/oclcl)
to help translate Common Lisp programs to OpenCL.

## What works?

We can translate some code like:

```lisp
(compute (α #'+ #(1 2 3) #(2 3 4))) => #(3.0 5.0 7.0)

(defun square (x) (* x x))
(compute (α #'square #(1 2 3 4 5))) => #(1.0 4.0 9.0 16.0 25.0)
(compute (α #'+ #(1 2) #2a((1 2) (3 4)))) => #2a((2.0 3.0) (5.0 6.0))

(compute (β #'+ #(1 2 3 4))) => 10.0

(defun matmul (A B)
  (β #'+
     (α #'*
        (reshape A (τ (m n) (n m 0)))
        (reshape B (τ (n k) (n 0 k))))))

(compute (matmul #2a((1 2)) #2a((1 2) (3 4)))) => #2a((7.0 10.0))
```

Currently, the only control structures that can be translated are IF and DO, 
numbers are coerced to floats, and only `petalisp:α` and `petalisp:β` are 
implemented. This is only slightly better than a proof-of-concept currently...

We need to implement (written as function/IR class):

- fuse/fusion
- reshape/reference on its own, it gets squished into applications
