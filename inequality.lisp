(defpackage :dyn/inequality
  (:use :cl :alexandria :float-features :parseq :dyn/domain)
  (:export :parse-inequalities))

(in-package :dyn/inequality)

(defun parse-inequalities (var list)
  (with-saved-rules
    (defrule r () (inequality-tree var))
    (reduce #'domain-intersection (mapcar (curry #'parseq 'r) list))))

(defrule inequality (variable) (or (left-inequality variable) (right-inequality variable)))

;;; These should really accept an arbitrary number of args like lisp and/or.
(defrule inequality-and (variable) 
  (list 'and (inequality-tree variable) (inequality-tree variable)) (:choose 1 2) (:function #'domain-intersection))

(defrule inequality-or (variable) 
  (list 'or (inequality-tree variable) (inequality-tree variable)) (:choose 1 2) (:function #'domain-union))

(defrule inequality-tree (variable)
  (or (inequality variable)
      (inequality-and variable)
      (inequality-or variable)))


(defrule left-inequality (variable) (list sign variable number) (:choose 0 2) (:function #'inequality->domain))
(defrule right-inequality (variable) (list inverse-sign number variable) (:choose 0 1) (:function #'inequality->domain))

(defun inequality->domain (sign value)
  (ecase sign
    (= value)
    (< `((,single-float-negative-infinity . ,value)))
    (> `((,value . ,single-float-positive-infinity)))))


(defrule sign () (or '= '< '>))

(defrule inverse-sign () (or '= '< '>)
  (:lambda (s)
   (ecase s
     (< '>)
     (> '<)
     (= '=))))
