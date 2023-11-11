(defpackage :drom/grammar/drom
  (:use :cl :alexandria :float-features :parseq :drom/domain)
  (:export :parse-inequalities :parse-inequality))

(defrule rule ()
         (or declaration
             formula))

(defule declaration () 'todo)

(defrule formula ()
  (or atomic-formula
      connective))

(defrule atomic-formula ()
         (or equation
             predicate))

(defrule equation ()
  (list equality-sign (rep (2 nil) expression)))

(defrule equality-sign () (or '< '= '>))

(defrule expression ()
  (or number
      variable
      operation))

(defrule variable ()
  symbol
  (:external vars)
  (:test (v) (find v vars)))

(defrule operation ()
  (or plus minus times divide expt)) 

(defrule plus () (binary+ '+ expression))
(defrule minus () (binary+ '- expression))
(defrule times () (binary+ '* expression))
(defrule divide () (binary+ '/ expression))
(defrule expt () (binary+ '** expression)) 


(defrule predicate ()
  (list symbol (* expression)))


(defrule connective ()
  (or negation 
      conjunction
      disjunction))

(defrule negation ()
  (list 'not formula)
  (:choose 1)
  (:function #'domain-inverse))

(defrule conjunction ()
  (list 'and formula formula)
  (:choose 1 2)
  (:function #'domain-intersection))

(defrule disjunction ()
  (list 'or formula formula)
  (:choose 1 2)
  (:function #'domain-union))




(defrule unary (operator operand)
  (list operator operand)
  (:choose 1))

(defrule binary (operator)
  (list operator operand operand)
  (:choose 1 2))

(defrule ternary (operator)
  (list operator operand operand operand)
  (:choose 1 2 3))

(defrule nary (operator)
  (list operator (rep (1 nil) operand))
  (:choose 1))

(defrule binary+ (operator)
  (list operator (rep (2 nil) operand))
  (:choose 1))
