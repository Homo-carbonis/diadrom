(in-package #:dyn)

;;; Example 1
;;; A simple pendulum modeled as a particle at a fixed length from the origin.
(defsys pendulum (length theta) ((bob (particle)))
  (= theta (angle y-axis bob))
  (= (bob :r) length)
  (gravity bob))

(defsys particle (v x y z r rho theta phi) ()
  (= x (elt v 1))
  (= y (elt v 2))
  (= z (elt v 3))
  (= x (* r (sin theta) (cos phi)))
  (= y (* r (sin theta) (sin phi)))
  (= z (* r  (cos theta)))
  (= r (sqrt (+ (sqr x) (sqr y) (sqr z))))
  (= rho (sqrt (+ (sqr x) (sqr y)))))

(defrule gravity (body)
  (force 9.81  body))

(defrule force (magnitude body)
  (= (d (body :v)))

;; The same but for concision using the name of a system class (ie. 'particle') as the name the subsystem.
(defsys pendulum (length theta) ((particle))
  (= theta (angle y-axis particle))
  (= (distance pendulum origin) length)
  (gravity pendulum))

;; Some applications of the system

(pendulum 1 0.43)

(pendulum 10)

(pendulum :length 3)

(pendulum :length 1 :theta 0.5pi)

(pendulum :length 1 :theta 0.5pi)

; Initial conditions

(pendulum :length 1 :initial (:theta 0.23))

;; These can then be queried to get results:

(let ((p (pendulum 1 :initial (:theta 0.23) :t 1m)))
  (print (p :theta)) ; Standard syntax
  (print (p)) ; :theta can also be inferred since all other variables are specified.
  )

;;; Example 2
;;; Population

(defsys growth (n rate) ()
  (= (d n) ((* rate n))))

;;; Example 3
;;; Bodies
(defsys body

;;; Example 4
;;; Playing cards

(defsys card (suit value) ()
  (in suit (set diamonds hearts spades clubs nil))
  (or (in value (ordered-set ace 1 2 3 4 5 6 7 8 9 10 jack queen king))
      (= value joker))
  (when (= value joker) (= suit nil)))

(defsys pack (n) (((card) :repeat n))
  (when (not (= (value card) joker)) (unique card))
  (count (= (value card) joker) 2)
  (max n))

;OR

(defset pack card
  (when (not (= (value card) joker)) (unique card))
  (count (= (value card) joker) 2)
  (max n)) ; Anaphor n for size of set

; Maybe focus on continuous dynamical systems


;;; Design Points

;; defsys macros are a wrapper around rules language
; (defsys (x) () ...), when applied, will expand to: ((= x ...) ...)


;; Maybe try to simplify lisp syntax and standards where possible since we are not writing general purpose programs and potential users may not be lispers. Try not to disrupt standard CL in doing so though.
; Interpret an unquoted list as a quoted list where the first element does not represent a function.
; Allow = to be used for any equality test and try to do the right thing.