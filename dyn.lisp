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
(named-readtables:in-readtable lol:lol-syntax)

(defmacro defsys (name lambda-list subsystems &body rules)
  `(defun ,name ,lambda-list
     (let ,(mv-mapcar #`(,a1 (,a1 ,a2)) (unzip2 subsystems))
       (solve ,@rules))))
     
     
  `(defmacro! (symbolicate "MAKE-" name) ,variables
     (let ((g!subsystems
             ))
       `(let ((,g!rules ',',rules)
        ,@(mapcar #2`(nsubst ,a1 ',a2 ,g!rules) (list ,@variables) ',variables)
        (nconc ,g!rules ,@g!subsystems))))))

(defun subsystems (bindings subsystems)
  (mapcan (lambda (subsystem) 
  (mvbind (names sub-vars) (unzip2 subsystems)
          (mapcan (lambda (n) 
          (zip names
               (nsubst* bindings sub-vars))))


(defun nsubst* (substitutions tree)
  (dolist (s substitutions)
     (nsubst (cadr s) (car s) tree)
     
     

; (mapcar #``(,(symbol-suffix (car ,a1) "-RULES") ,(cdr ,a1)) ',subsystems)

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


(defun find-explicit-range (component rules)
  (let ((upper (find-explicit-relation '> component rules))
        (lower (find-explicit-relation '< component rules)))
    (if (and upper lower) (cons upper lower))))

(defun find-explicit-relation (relation component rules)
   "Return explicitly specified relation."
   (some (lambda (r)
           (match r ((explicit-relation relation component) value)))
         rules))

(defpattern explicit-relation (relation component)
    `(or
      (guard (list r c (and value (type number)))
             (and (eq r ,relation) (eq c ,component)))
      (guard (list r (and value (type number)) c)
             (and (eq r (inverse ,relation)) (eq c ,component)))))




(defun inverse (relation)
  (case relation
    (< '>)
    (> '<)
    (= '=)))