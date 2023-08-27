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
    (mapc (curry #'add-vertex *graph*) vars)
    (add-edge *graph* rule vars)))

(defun parse-variables (form)
  (let ((vars nil))
    (labels ((parse (cdr)
               (dolist (x cdr vars)
                 (cond ((symbolp x) (pushnew x vars))
                       ((consp x) (parse (cdr x)))))))
      (parse (cdr form)))))

(defpattern explicit-relation (relation component)
    `(or
      (guard (list r c (and value (type number)))
             (and (eq r ,relation) (eq c ,component)))
      (guard (list r (and value (type number)) c)
             (and (eq r (inverse ,relation)) (eq c ,component)))))

(defmacro solve (components &body rules)
  (let* ((bounds (mapcar (rcurry #'find-explicit-range rules) components)))
    `(n-tree-search (lambda ,components (and ,@rules)) ',bounds *depth-limit*)))

(defun find-explicit-range (component rules)
  (let ((upper (find-explicit-relation '> component rules))
        (lower (find-explicit-relation '< component rules)))
    (if (and upper lower) (cons upper lower))))

(defun find-explicit-relation (relation component rules)
  "Return explicitly specified relation."
  (some (lambda (r)
          (match r ((explicit-relation relation component) value)))
        rules))



(defun inverse (relation)
  (case relation
    (< '>)
    (> '<)
    (= '=)))