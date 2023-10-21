(defpackage :dyn/n-tree 
  (:use :cl :alexandria :utils/misc)
  (:export :n-tree-search))

(in-package :dyn/n-tree)

(defun n-tree-search (test bounds limit)
  "Perform a breadth-first n-dimensional tree search (ie. bintree, quadtree, octree, etc. depending on the length of 'bounds') for a point satisfying 'test'. Return nil if a result is not found within 'limit' branches"
  (loop for i below limit
        with next-level = (list bounds)
        with midpoints
        do (setf midpoints (mapcar #'midpoint next-level))
        thereis (some-value (curry #'apply test) midpoints)
        do (setf next-level (mapcan #'subdivide next-level midpoints))))


(defun midpoint (bounds)
  (mapcar #'mid bounds))

(defun mid (pair)
  (/ (+ (car pair) (cdr pair)) 2))

(defun subdivide (bounds point)
  "Subdivide an n-cuboid into n parts at 'point'. Return a list of lists of bounds"
  (apply #'map-product #'list 
               (mapcar #'divide-range bounds point)))

(defun divide-range (range point)
  (list (cons (car range) point) (cons point (cdr range))))
  
  
(defun product (&rest lists)
  "Return the Cartesian product of lists."
  (if (cdr lists)
      (mapcan (lambda (a)
                (mapcar (lambda (b) (cons a b))
                        (apply #'product (cdr lists))))
              (car lists))
      (mapcar #'list (car lists))))


