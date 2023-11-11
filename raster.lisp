(defpackage :drom/raster
  (:use :cl :drom/domain)
  (:import-from :utils/misc :on :this :*epsilon*)
  (:import-from :alexandria :map-product :iota)
  (:export :rasterise :raster-map))

(in-package :drom/raster)
(defun rasterise (function domain resolution)
  "Return an n-dimensional array of values of n-ary function.
   Domain     --- a list of length n containg intervals for each dimension.
   Resolution --- a list of length n containing the resolution for each dimension."
  (on (make-array resolution)
      (map-product-iota
        (lambda (&rest indices)
          (let ((values (mapcar
                          (lambda (index interval n)
                            (+ (lower interval) (* index (/ (- (upper interval) (lower interval)) n))))
                          indices domain resolution)))
            (setf (apply #'aref this indices) (apply function values))))
        resolution)))

(defun raster-map (function domain resolution)
  "Map a function over an n-dimensional space.
   function --- (function indices values)
   indices --- a list of n indices
   values --- a list of n values
   Domain     --- a list of length n containg intervals for each dimension.
   Resolution --- a list of length n containing the resolution for each dimension."
  (let* ((deltas
          (mapcar
            (lambda (interval n)
              (/ (- (upper interval) (lower interval)) n))
            domain resolution))
        (*epsilon* (car (last deltas))))
    (map-product-iota
      (lambda (&rest indices)
        (let ((inputs (mapcar
                        (lambda (index interval delta)
                          (+ (lower interval) (* index delta)))
                        indices domain deltas)))
          ;; The dynamic binding of *epsilon* is set so approx= will use the appropriate precision.
          (funcall function indices inputs)))
      resolution)))

(defun map-product-iota (f dimensions)
  (apply #'map-product f (mapcar #'iota dimensions)))
