;;; An interval is a cons of the minimum and maximum values.
;;; TODO: Sort out open/closed intervals.

(defpackage drom/interval
  (:use :cl :float-features)
  (:import-from :utils/misc :with-minmax)
  (:export :interval
           :make-interval
           :ensure-interval
           :inf
           :sup
           in-interval

           :interval-relation
           :a<b
           :a-intersects-b
           :b-subset-a
           :a-subset-b
           :b-intersects-a
           :a>b

           :subintervalp
           :map-interval
           :step-interval))

(in-package :drom/interval)

(defstruct (interval (:conc-name nil))
  (inf single-float-negative-infinity :type single-float)
  (sup single-float-positive-infinity :type single-float))

(defun ensure-interval (i)
  "Return i if i is an interval or make an interval if i is a single value."
  (if (consp i)
      i
      (make-interval :inf i :sup i)))

(defun range (interval)
  (- (sup interval) (inf interval)))

(defun in-interval (x interval)
  (and (> x (inf interval))
       (< x (sup interval))))

(defun interval-relation (a b)
  (if (< (inf a) (inf b))
      (if (< (sup a) (inf b))
          'a<b
          (if (< (sup a) (sup b))
              'a-intersects-b
              'b-subset-a))
      (if (< (inf a) (sup b))
          (if (< (sup a) (sup b))
              'a-subset-b
              'b-intersects-a)
          'a>b)))

(defun subintervalp (a b)
  (and (< (inf b) (inf a))
       (> (sup b) (sup a))))

(defun map-interval (f &rest intervals)
  "Apply f to the inf and sup bounds of `intervals` and make a new inverval from the two results."
  (let ((args-inf (mapcar #'inf intervals))
        (args-sup (mapcar #'sup intervals)))
    (with-minmax ((apply f args-inf) (apply f args-sup))
      (make-interval :inf min :sup max))))
 
(defun step-interval (interval)
  (make-interval :inf (sup interval) :sup (+ (sup interval) (range interval))))
