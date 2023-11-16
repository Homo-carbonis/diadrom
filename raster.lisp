(defpackage :drom/raster
  (:use :cl :drom/interval)
  (:import-from :utils/misc :on :this :with-if :then :*epsilon*)
  (:import-from :alexandria :map-product :iota :rcurry :ensure-function :lastcar)
  (:export :raster-map :raster-map-1))

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

(defun raster-map-interval (map-function function domain resolution)
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
                   (loop for x1 = lower then x2
                         for x2 = (+ x1 delta)
                         for x = (make-interval :lower x1 :upper x2)
                         for i below n
                         for %indices = (cons i indices)
                         do (then (%raster-map (rcurry f x) domain resolution deltas %indices)
                                  (funcall map-function %indices (funcall f x))))))))
      (%raster-map (ensure-function function) domain resolution deltas nil))))

(defun raster-map-1 (map-function function domain resolution)
  (let ((f (lambda (&rest args)
             (let* ((y-interval (apply #'map-interval function (butlast args)))
                    (y (lower (lastcar args))))
               (in-interval y y-interval)))))
    (raster-map-interval map-function f domain resolution)))


