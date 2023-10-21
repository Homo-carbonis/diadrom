(defpackage :dyn/inequality
  (:use :cl :parseq))

(in-package :dyn/inequality)

(defrule inequality-tree (variable) (or inequality
                                        ('and inequality-tree inequality-tree)
                                        ()
                                        ))

(defrule inequality (variable) (or left-inequality right-inequality))
(defrule left-inequality (variable) (and sign variable value) (:choose 0 2) (:function #'inequality->domain))
(defrule right-inequality (variable) (and inverse-sign value variable) (:choose 0 1) (:function #'inequality->domain))

(defun inequality->domain (sign value)
  (ecase sign
    (= value)
    (< (cons 'inf value))
    (> (cons value 'inf))))


(defrule sign () (or '= '< '>))
(defrule inverse-sign () (or '= '< '>)
  (:lambda (s)
   (ecase s
     (< '>)
     (> '<)
     (= '=))))

(defrule and () ('and inequality-tree inequality-tree) (:lambda (a b) ))
