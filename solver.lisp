(in-package #:dyn)

#|
Summary of the solver algorithm:

1. Build a hyper-graph of relations.
Collect symbols from each rule and assign to vertices. Rules themselves are assigned to hyper-edges.
2. Propagate constraints, restricting the domain of each variable to the smallest possible interval.
3. If an explicit solution is not found, perform n-tree search, where n is the number of unknown variables.
|#

(defvar *graph* (make-graph))

(defun parse-rule (rule)
  (let ((vars (parse-variables rule)))
    (dolist (v vars) (add-vertex *graph* v))
    (add-edge *graph* rule vars)))

(defun parse-variables (form)
  (let ((vars nil))
    (labels ((parse (cdr)
               (dolist (x cdr vars)
                 (cond ((symbolp x) (pushnew x vars))
                       ((consp x) (parse (cdr x)))))))
      (parse (cdr form)))))