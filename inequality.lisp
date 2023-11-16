(defpackage :drom/inequality
  (:use :cl :alexandria :float-features :parseq :drom/domain)
  (:export :parse-inequalities :parse-inequality))

(in-package :drom/inequality)

(defun parse-inequalities (var list)
  (reduce #'domain-intersection (remove nil (mapcar (curry #'parse-inequality var) list))))

(defun parse-inequality (var form)
  (parseq 'unary-inequality-tree (list var form)))

(defrule unary-inequality-tree ()
         (and set-var inequality-tree)
         (:choose 1)
         (:let var))

(defrule set-var ()
  symbol
  (:external var)
  (:lambda (v) (setf var v)))

(defrule get-var ()
  symbol
  (:external var)
  (:test (v) (eq v var)))

(defrule inequality () (or left-inequality right-inequality))

;;; These should really accept an arbitrary number of args like lisp and/or.
(defrule inequality-and () 
  (list 'and inequality-tree inequality-tree) (:choose 1 2) (:function #'domain-intersection))

(defrule inequality-or () 
  (list 'or inequality-tree inequality-tree) (:choose 1 2) (:function #'domain-union))

(defrule inequality-tree ()
  (or inequality 
      inequality-and 
      inequality-or))



(defrule left-inequality ()
  (list sign get-var number) (:choose 0 2) (:function #'inequality->domain))

(defrule right-inequality ()
  (list inverse-sign number get-var) (:choose 0 1) (:function #'inequality->domain))

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
