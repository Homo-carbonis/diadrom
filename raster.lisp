(defpackage :drom/raster
  (:use :cl :drom/interval)
  (:import-from :utils/misc :on :this :with-if :then :*epsilon*)
  (:import-from :alexandria :map-product :iota :rcurry :ensure-function :lastcar)
  (:import-from :serapeum :nlet)
  (:export :raster-map :raster-map-1))

(in-package :drom/raster)

(defstruct (rastrum (:conc-name nil) (:constructor make-rastrum (resolution interval))) 
  (resolution 0 :type integer)
  (interval (make-interval) :type interval))

(defun make-box (resolutions intervals)
  (mapcar #'make-rastrum resolutions intervals ))

(defun delta (interval n)
  (/ (- (sup interval) (inf interval)) n))

(defmacro do-raster ((box small-box &optional (indices nil indices-p)) &body body)
  `(flet ((f (small-box ,(when indices-p indices)) ,@body))
     (nlet raster ((box (reverse ,box)) (small-box nil) ,(when indices-p '(indices nil)))
           (let* ((n (resolution box))
                  (delta (delta (resolution box)))
                  (interval (interval (car box)))
                  (inf (inf interval)))
             (with-if box
                      (loop for x1 = inf then x2
                            for x2 = (+ x1 delta)
                            for x = (make-interval :inf x1 :sup x2)
                            for i below n
                            do (then (raster (cdr box) (cons x ,small-box) ,(when indices-p '(cons i indices)))
                                     (f ,small-box ,(when indices-p '(cons i indices))))))))))

