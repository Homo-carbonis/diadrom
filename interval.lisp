;;; An interval is a cons of the minimum and maximum values.
;;; TODO: Sort out open/closed intervals.

(defpackage :drom/interval
  (:use :cl :utils/misc :float-features)
  (:export :make-interval
           :lower
           :upper
           in-interval

           :interval-relation
           :a<b
           :a-intersects-b
           :b-subset-a
           :a-subset-b
           :b-intersects-a
           :a>b

           :map-interval
           :step-interval))

(in-package :drom/domain)

(defun make-interval (&key (lower single-float-negative-infinity) (upper single-float-positive-infinity))
  (cons lower upper))

(defun lower (interval) (car interval))
(defun upper (interval) (cdr interval))

(defun range (interval)
  (- (upper interval) (lower interval)))

(defun in-interval (x interval)
  (and (> x (car interval))
       (< x (cdr interval))))

(defun interval-relation (a b)
  (if (< (car a) (car b))
      (if (< (cdr a) (car b))
          'a<b
          (if (< (cdr a) (cdr b))
              'a-intersects-b
              'b-subset-a))
      (if (< (car a) (cdr b))
          (if (< (cdr a) (cdr b))
              'a-subset-b
              'b-intersects-a)
          'a>b)))

(defun map-interval (f &rest intervals)
  (let ((lower-bounds (mapcar #'lower intervals))
        (upper-bounds (mapcar #'upper intervals))))
  (make-interval :lower (apply f lower-bounds)
                 :upper (apply f upper-bounds)))
 
(defun step-interval (interval)
  (make-interval :lower (upper interval) :upper (+ upper (range interval))))
