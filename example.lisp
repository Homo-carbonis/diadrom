(in-package #:dyn)

;;; Example 1
;;; A simple pendulum modeled as a particle at a fixed length from the origin.
(defsys pendulum () (length theta) ((particle :r length :phi theta (gravity))))

(defsys point () (v x y z r rho theta phi) ()
  (= x (elt v 1))
  (= y (elt v 2))
  (= z (elt v 3))
  (= x (* r (sin theta) (cos phi)))
  (= y (* r (sin theta) (sin phi)))
  (= z (* r  (cos theta)))
  (= r (sqrt (+ (sqr x) (sqr y) (sqr z))))
  (= rho (sqrt (+ (sqr x) (sqr y)))))

(defsys particle (point) (v x y z r rho theta phi mass force) ()
  (= (d v) (/ force mass)))

(defrule gravity ()
  (add-force #(0 -9.81 0) force))

;; Result (names will really be gensyms)

(= x (elt v 1))
(= y (elt v 2))
(= z (elt v 3))
(= x (* length (sin theta) (cos theta1)))
(= y (* length (sin theta) (sin theta1)))
(= z (* length  (cos theta)))
(= length (sqrt (+ (sqr x) (sqr y) (sqr z))))
(= rho (sqrt (+ (sqr x) (sqr y))))
(add-force #(0 -9.81 0) force)
(= (d v) (/ force mass)))

; The most complex part is deducing the tension force required to keep length constant.

;; Some queries of the system

(pendulum 1 0.43)

(pendulum 10)

(pendulum :length 3)

(pendulum :length 1 :theta 0.5pi)

(pendulum :length 1 :theta 0.5pi)

; Initial conditions

(pendulum :length 1 :initial (:theta 0.23))

;; These can then be queried to get results:

(let ((p (pendulum 1 :initial (:theta 0.23) :t 1m)))
  (print (theta p)) ; Standard syntax
  (print (p))) ; :theta can also be inferred since all other variables are specified.

;;; Example 1b
;;; Double Pendulum
(defsys pendulum () (length theta v) ((particle :r length :phi theta :v v (gravity))))

(defsys double-pendulum () (length-1 length-2 theta-1 theta-2 v1 v2)
  ((pendulum :length length-1 :theta theta-1 :v v1)
   (pendulum :length length-2 :theta theta-2 :v v2 (origin v1))))

; Here three tension forces must be inferred. Two on the first particle and one on the second. This may be too much for the solver to handle. Some (add-force ...) rules might be required to make explicit the tension forces. 

;;; Example 2
;;; Exponential growth

(defsys growth () (x rate) ()
  (= (d n) ((* rate n))))

;;; Example 3
;;; n-body problem
(defsys bodies (n) (xs ms)
  (#n*(particle :v #xs :m #ms :xs (remove #xs xs) :ms (remove #ms ms)
       (= force (+ #*(/ (* (- #xs v) G m #ms) (sqr (distance v #xs))))))))
; This one is a lot more complicated. The number of bodies is bound to n and their positions and masses to the vectors xs and ms. The #* read macro repeats a form. Within this form #x refers to the i-th element of the vector #x. The number of repeats is either specified as #n* or inferred from the length of the vectors supplied to #.
; #* is used twice. Once for each particle and once within each particle to sum the attractive forces from all the other particles.
; This may well need some revision.

;;; Result



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

; Maybe focus on continuous dynamical systems. Combinatorics is probably outside the scope of dyn (at least for now).


;;; Design Points

;; defsys macros are a wrapper around rules language
; (defsys (x) () ...), when applied, will expand to: ((= x ...) ...)
