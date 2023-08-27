(in-package #:hypergraph)

(defun make-graph ()
  (make-hash-table))

(defun add-vertex (graph key)
  (setf (gethash key graph) nil))

(defun add-edge (graph value &rest vertices)
  (let ((edge (cons value vertices)))
    (dolist (v vertices)
      (pushnew edge (gethash v graph)))))
 
(defun test ()
  (let* ((g (make-graph)))
    (add-vertex g 1)
    (add-vertex g 2)
    (add-edge g 'a 1 2)
    g))
               