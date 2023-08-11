(in-package #:dyn)


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