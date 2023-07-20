; Examples
; (def-component cell () ((point)) (eq (distance (nearest cell)) 1))
; (cell)
; (alias-component point (dimension v))
; (defun distance (a b) (magnitude (- (point b) (point a))))
; (defun nearest (a) (min (amap (distance a a1) (family a))))  

; Example component lists
; (5 point)
; ((point) r)


(in-package #:dyn)

;(defvar systems (make-hash-table))

;(defun component-slot (component)
;  (trivia:match component
;    ((list name initform) `(,name :initform ,initform))
;    ((list name sub-component parameters) `(,name :initform (,sub-component ,parameters)))
;    (name `(,name))))
;
;(defun component-slots (form)
;  (if (numberp (carr form))
;      (let* ((n (car form))
;             (slot-spec (component-slot (cadr form)))
;             (name (car slot-spec))
;             (rest (cdr slot-spec)))
;        (maptimes (lambda (i) (cons (symbol-number name i) rest)) n))
;      `(,(component-slot form))))

;(defmacro defsys (name superclasses components parameters &body rules)
;  (let parameters
;    (solve components rules)))

;; Example rules
; (= a b)
; (= (foo a) (bar b))
; (< x (+ 2 (foo y)))
; (foo-p x)
; (primep num)

; Write solvers from most generic to most specialized. Evaluate in reverse order until solution is found.

; Most General solver: discretization solution. Subdivide domain and evaluate constraints for every element. Slow or inaccurate but universally applicable. 
; the resulting linear system.
  
(defparameter *depth-limit* 10)

(defmacro solve (components &body rules)
  (let* ((components (list-if components))
         (bounds (mapcar (rcurry #'find-explicit-range rules) components)))
    `(n-tree-search (lambda ,components (and ,@rules)) ',bounds *depth-limit*)))

(defpattern explicit-relation (relation component)
    `(guard (list r c (and value (type number))) (and (eq r ,relation) (eq c ,component))))

(defun find-explicit-relation (relation component rules)
  "Return explicitly specified relation."
  (some (lambda (r)
          (match r ((explicit-relation relation component) value)))
        rules))


(defun find-explicit-range (component rules)
  (let ((upper (find-explicit-relation '> component rules))
        (lower (find-explicit-relation '< component rules)))
    (if (and upper lower) (cons upper lower))))
