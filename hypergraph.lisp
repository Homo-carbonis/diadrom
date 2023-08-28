(in-package #:hypergraph)

(defun make-graph ()
  (make-hash-table))

(defun add-vertex (graph key &optional value)
  (setf (gethash key graph) (cons value nil)))

(defsetf vertex (graph vertex) (value)
  `(setf (car (gethash ,vertex ,graph)) ,value))

(defun add-edge (graph value vertices)
  (let ((edge (cons value vertices)))
    (dolist (v vertices)
      (pushnew edge (gethash v graph)))))
 
(defun test ()
  (let* ((g (make-graph)))
    (add-vertex g 1)
    (add-vertex g 2)
    (add-edge g 'a '(1 2))
    (setf (vertex g 1) 5)
    g))
               