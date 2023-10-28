(defpackage :drom/solver
  (:use :cl :hypergraph :drom/n-tree :drom/domain :drom/inequality))

(in-package :drom/solver)

#|
Summary of the solver algorithm:

1. Build a hyper-graph of relations.
Collect symbols from each rule and assign to vertices. Rules themselves are assigned to hyper-edges.
2. Propagate constraints, restricting the domain of each variable to the smallest possible interval.
3. If an explicit solution is not found, perform n-tree search, where n is the number of unknown variables.
|#

(defun parse-rules (rules)
  (let ((graph (make-ve-graph)))
    (dolist (r rules graph)
      (parse-rule r graph))
    graph))

(defun parse-rule (rule graph)
  (let ((vars (parse-variables rule)))
    (dolist (v vars) (add-vertex graph :key v :value (make-domain)))
    (add-edge graph :vertices vars :value rule)))

(defun parse-variables (form)
  (let ((vars nil))
    (labels ((parse (cdr)
               (dolist (x cdr vars)
                 (cond ((symbolp x) (pushnew x vars))
                       ((consp x) (parse (cdr x)))))))
      (parse (cdr form)))))

(defun make-unary-consistant (vertex graph)
  (setf (vertex-value vertex graph)
        (parse-inequalities vertex (vertex-nary-edge-values 1 vertex graph))))
