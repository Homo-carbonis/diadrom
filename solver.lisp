(in-package #:dyn)

#|
Summary of the solver algorithm:

1. Build a hyper-graph of relations.
Collect symbols from each rule and assign to vertices. Rules themselves are assigned to hyper-edges.
2. Propagate constraints, restricting the domain of each variable to the smallest possible interval.
3. If an explicit solution is not found, perform n-tree search, where n is the number of unknown variables.
|#

(defun parse-rules (rules)
  (let ((graph (make-graph (* 2 (length rules)))))
    (dolist (r rules graph)
      (parse-rule graph r))))

(defun parse-rule (graph rule)
  (let ((vars (parse-variables rule)))
    (dolist (v vars) (add-vertex graph v))
    (add-edge graph rule vars)))

(defun parse-variables (form)
  (let ((vars nil))
    (labels ((parse (cdr)
               (dolist (x cdr vars)
                 (cond ((symbolp x) (pushnew x vars))
                       ((consp x) (parse (cdr x)))))))
      (parse (cdr form)))))

(defun make-unary-consistant (graph vertex)
  (setf (vertex-value graph v)
        (find-explicit-domain vertex (vertex-nary-edge-values graph vertex 1))))

(defpattern explicit-bound (sign component)
    `(or
      (guard (list s c (and value (type number)))
             (and (eq s ,sign) (eq c ,component)))
      (guard (list s (and value (type number)) c)
             (and (eq s (inverse ,sign)) (eq c ,component)))))

(defun find-explicit-domain (component rules)
  (let ((upper (find-explicit-bound '> component rules))
        (lower (find-explicit-bound '< component rules)))
    (if (and upper lower) (cons upper lower))))

(defun find-explicit-bound (sign component rules)
  "Return explicitly specified bound."
  (some (lambda (r)
          (match r ((explicit-bound sign component) value)))
        rules))

(defun inverse (sign)
  (case sign
    (< '>)
    (> '<)
    (= '=)))
