(defpackage :drom/raster
  (:use :cl :drom/domain)
  (:import-from :utils/misc :on :this)
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

(defun raster-map (map-function function domain resolution)
  "Return an n-dimensional array of values of n-ary function.
   Domain     --- a list of length n containg intervals for each dimension.
   Resolution --- a list of length n containing the resolution for each dimension."
  (map-product-iota
    (lambda (&rest indices)
      (let ((values (mapcar
                      (lambda (index interval n)
                        (+ (lower interval) (* index (/ (- (upper interval) (lower interval)) n))))
                      indices domain resolution)))
        (funcall map-function indices (apply function values))))
    resolution))

(defun raster-every (map-function function domain resolution)
  "Return an n-dimensional array of values of n-ary function.
   Domain     --- a list of length n containg intervals for each dimension.
   Resolution --- a list of length n containing the resolution for each dimension."
  (map-product-iota
    (lambda (&rest indices)
      (let ((values (mapcar
                      (lambda (index interval n)
                        (+ (lower interval) (* index (/ (- (upper interval) (lower interval)) n))))
                      indices domain resolution)))
        (if (approx= (funcall fun......)))
        (funcall map-function indices (apply function values))))
    resolution))


(defun map-product-iota (f dimensions)
  (apply #'map-product f (mapcar #'iota dimensions)))
