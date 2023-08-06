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

(defmacro defsys (name components subsystems &body rules)
  `(defun ,name ,components
     (flet ,(mapcar destructuring-bleh subsystems)
       (solve ,components ,@rules))))

(defparameter *depth-limit* 10)
