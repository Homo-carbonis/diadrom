; Examples
; (def-component cell () ((point)) (eq (distance (nearest cell)) 1))
; (cell)
; (alias-component point (dimension v))
; (defun distance (a b) (magnitude (- (point b) (point a))))
; (defun nearest (a) (min (amap (distance a a1) (family a))))  

; Example component lists
; (5 point)
; ((point) r)

(defpackage #:dyn
  (:use #:cl #:misc #:trivia #:lol)
  ;(:export #:defsys))
)
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
  

(defmacro! solve (components &body rules)
  `(let ((,g!bounds (mapcar (rcurry #'find-explicit-range ,rules) ,components)))
    (n-tree-search (lambda ,components (and ,@rules)) ,g!bounds *depth-limit*)))

(defpattern explicit-bound (relations component)
    `(list (and relation (or ,@relations)) ,component (and bound (type number))))

(defun find-explicit-min (component rules)
  "Return explicitly specified lower bound. Also return t if bound is open and nil if closed."
  (some (lambda (r) (match r
                      ((explicit-bound '(> >=) component)
                       (values bound (eq relation '>=)))))
        rules))


(defun find-explicit-max (component rules)
  "Return explicitly specified upper bound. Also return t if bound is open and nil if closed."
  (some (lambda (r) (match r
                      ((explicit-bound '(< <=) component)
                       (values bound (eq relation '<= t)))))
        rules))

(defun find-explicit-range (component rules)
  (mvbind (min min-open) (find-explicit-min component rules)
          (mvbind (max max-open) (find-explicit-max component rules)
                  (values (cons min max) (cons min-open max-open)))))
                  

