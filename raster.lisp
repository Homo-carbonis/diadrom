(defpackage :drom/raster
  (:use :cl :drom/domain)
  (:import-from :utils/misc :on :this :with-if :then :*epsilon*)
  (:import-from :alexandria :map-product :iota :rcurry :ensure-function)
  (:export :raster-map))

(in-package :drom/raster)

(defun delta (interval n)
  (/ (- (upper interval) (lower interval)) n))

(defun raster-map (map-function function domain resolution)
  (let* ((domain (reverse domain))
         (resolution (reverse resolution))
         (deltas (mapcar #'delta domain resolution)))
   (labels ((%raster-map (f domain resolution deltas indices)
             (let* ((interval (car domain))
                    (domain (cdr domain))
                    (lower (lower interval))
                    (n (car resolution))
                    (resolution (cdr resolution))
                    (delta (car deltas))
                    (deltas (cdr deltas)))
               (with-if domain
                        (loop for x from lower by delta 
                              for i below n
                              for %indices = (cons i indices)
                              do (then (%raster-map (rcurry f x) domain resolution deltas %indices)
                                       (funcall map-function %indices (funcall f x))))))))
    (%raster-map (ensure-function function) domain resolution deltas nil))))
