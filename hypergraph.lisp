(in-package #:hypergraph)

(defun make-graph ()
  (make-hash-table))

(defun add-vertex (graph key &optional value)
  (setf (gethash key graph) (cons value nil)))

(defun vertex-value (graph vertex)
  (car (gethash vertex graph)))

(defsetf vertex-value (graph vertex) (value)
  `(setf (car (gethash ,vertex ,graph)) ,value))

(defun vertex-edges (graph vertex)
  (cdr (gethash vertex graph)))

(defun add-edge (graph value vertices)
  (let ((edge (cons value vertices)))
    (dolist (v vertices)
      (pushnew edge (gethash v graph)))))

(defun edge-value (edge)
  (car edge))

(defun edge-vertices (edge)
  (cdr edge))

(defun edge-length (edge)
  (length (edge-vertices edge)))

(defun vertex-nary-edges (graph vertex n)
  "Return a list of edges connected to exactly `n` vertices (including `vertex`)"
  (remove-if-not (lambda (e) (= (edge-length e) n)) (vertex-edges graph vertex)))



  
(defun test ()
  (let* ((g (make-graph)))
    (add-vertex g 1)
    (add-vertex g 2)
    (add-edge g 'a '(1 2))
    (setf (vertex-value g 1) 5)
    g))
               