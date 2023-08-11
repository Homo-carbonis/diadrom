(in-package #:dyn)

;;; Example 1
;;; A simple pendulum modeled as a particle at a fixed length from the origin.

(defsys pendulum (length theta) ((bob (particle)))
  (= theta (angle y-axis bob))
  (= (distance bob origin) length)
  (gravity bob))

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
;;; 

;;; Design Points

;; Dimensionality should be transparent. A `particle` object should represent an n-dimensional particle and n should be determined automatically. Some special systems or objects undefinable in dyn itself will be needed to allow this.

; There needs to be a a way to handle mutually dependent variables. For instance a particle can have polar and cartesian coordinate values.