(defpackage :drom/raster
  (:use :cl :drom/interval)
  (:import-from :utils/misc :on :this :with-if :then :*epsilon*)
  (:import-from :alexandria :map-product :iota :rcurry :ensure-function :lastcar)
  (:import-from :serapeum :nlet)
  (:export :raster-map :raster-map-1))

(in-package :drom/raster)

(defstruct (rastrum (:conc-name nil)) 
  (interval (make-interval) :type interval)
  (resolution 0 :type integer))


(defun delta (interval n)
  (/ (- (sup interval) (inf interval)) n))

(defun raster-map (map-function function box resolution)
  (let* ((box (reverse box))
         (resolution (reverse resolution))
         (deltas (mapcar #'delta box resolution)))
    (labels ((%raster-map (f box resolution deltas indices)
               (let* ((interval (car box))
                      (box (cdr box))
                      (inf (inf interval))
                      (n (car resolution))
                      (resolution (cdr resolution))
                      (delta (car deltas))
                      (deltas (cdr deltas)))
                 (with-if box
                   (loop for x from inf by delta 
                         for i below n
                         for %indices = (cons i indices)
                         do (then (%raster-map (rcurry f x) box resolution deltas %indices)
                                  (funcall map-function %indices (funcall f x))))))))
      (%raster-map (ensure-function function) box resolution deltas nil))))

(defun raster-map-interval (map-function function box resolution)
  (let* ((box (reverse box))
         (resolution (reverse resolution))
         (deltas (mapcar #'delta box resolution)))
    (labels ((%raster-map (f box resolution deltas indices)
               (let* ((interval (car box))
                      (box (cdr box))
                      (inf (inf interval))
                      (n (car resolution))
                      (resolution (cdr resolution))
                      (delta (car deltas))
                      (deltas (cdr deltas)))
                 (with-if box
                   (loop for x1 = inf then x2
                         for x2 = (+ x1 delta)
                         for x = (make-interval :inf x1 :sup x2)
                         for i below n
                         for %indices = (cons i indices)
                         do (then (%raster-map (rcurry f x) box resolution deltas %indices)
                                  (funcall map-function %indices (funcall f x))))))))
      (%raster-map (ensure-function function) box resolution deltas nil))))

(defun raster-map-1 (map-function function box resolution)
  (let ((f (lambda (&rest args)
             (let* ((y-interval (apply #'map-interval function (butlast args)))
                    (y (inf (lastcar args))))
               (in-interval y y-interval)))))
    (raster-map-interval map-function f box resolution)))




(defmacro do-raster ((box small-box &optional (indices nil indices-p)) &body body)
  `(flet ((f (small-box ,(when indices-p indices)) ,@body))
     (nlet raster ((box (reverse ,box)) (small-box nil) ,(when indices-p '(indices nil)))
           (let* ((interval (interval (car box)))
                  (inf (inf interval))
                  (n (resolution box))
                  (delta (delta (resolution box))))
             (with-if box
                      (loop for x1 = inf then x2
                            for x2 = (+ x1 delta)
                            for x = (make-interval :inf x1 :sup x2)
                            for i below n
                            do (then (raster (cdr box) (cons x ,small-box) ,(when indices-p '(cons i indices)))
                                     (f ,small-box ,(when indices-p '(cons i indices)))))))))) ;; Do something about all the variables captured.

