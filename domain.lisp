;;; A domain is represented by a simple value, an interval, or an ordered list of intervals.
;;; An interval is a cons of the minimum and maximum values.
(defpackage :dyn/domain
  (:use :cl)
  (:export :domain-union
           :interval-relation))

(in-package :dyn/domain)

(defun domain-union (d1 d2)
  (reverse (%domain-union d1 d2 nil)))

(defun %domain-union (d1 d2 acc)
  (cond
    ((and d1 d2) 
     (let ((a (car d1))
           (b (car d2)))
       (ecase (interval-relation a b)
         (a<b
           (%domain-union (cdr d1) d2 (cons a acc)))
         (a-intersects-b
           (let ((u (cons (car a) (cdr b))))
             (%domain-union (cdr d1) (cons u (cdr d2)) acc)))
         (b-subset-a 
           (%domain-union d1 (cdr d2) acc)) 
         (a-subset-b
           (%domain-union (cdr d1) d2 acc))
         (b-intersects-a
           (let ((u (cons (car b) (cdr a))))
             (%domain-union (cons u (cdr d1)) d2 acc)))
         (a>b (%domain-union d1 (cdr d2) (cons b acc))))))
    (d1 (append d1 acc))
    (d2 (append d2 acc))
    (t acc)))


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

(defun in-interval (x interval)
  (and (> x (car interval))
       (< x (cdr interval))))
