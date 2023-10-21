(defpackage :dyn/solver
  (:use :cl :parseq :dyn/n-tree :hypergraph))

(in-package :dyn/solver)

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
      (parse-rule r graph))))

(defun parse-rule (rule graph)
  (let ((vars (parse-variables rule)))
    (dolist (v vars) (add-vertex graph :key v :value (list 'inf 'inf)))
    (add-edge graph :vertices vars :value rule)))

(defun parse-variables (form)
  (let ((vars nil))
    (labels ((parse (cdr)
               (dolist (x cdr vars)
                 (cond ((symbolp x) (pushnew x vars))
                       ((consp x) (parse (cdr x)))))))
      (parse (cdr form)))))


#|(defun make-unary-consistant (graph vertex)
  (setf (vertex-value graph vertex)
        (find-explicit-domain vertex (vertex-nary-edge-values graph vertex 1)))) |#

(defrule sign () (or '= '< '>))

(defrule inequality (variable) (or (and inequality-sign a b)))

(defpattern inequality (sign var value)
  `(or (list (sign ,sign) ,var (and ,value (type number)))
       (list (inverse ,sign) (and ,value (type number)) ,var)))

(defpattern and* (a b)
  `(list and ,a ,b))

(defpattern or* (a b)
  `(list or ,a ,b))
(defpattern not* (a)
  `(list not ,a))

(defpattern domain (var)
  (let ((sign (gensym))
        (value (gensym)))
    `(or (inequality ,sign ,var ,value)
         (and* (domain ,var) (domain ,var))
         (or* (domain ,var) (domain ,var)))))

(defun parse-ieq (var form)
  (match form ((inequality sign var) (cons s value))))

(defun invert (sign)
  (case sign
    (< '>)
    (> '<)
    (= '=)))
