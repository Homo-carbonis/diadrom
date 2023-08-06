(in-package #:dyn)
(defsys pendulum (length theta) ((bob particle))
  (= theta (angle y-axis bob))
  (= (distance bob origin) length)
  (gravity bob))

(pendulum 1 2)

(let ((p (pendulum 5)))
  (p :inital (:theta 10))
  (p 10))