;;; A domain is represented by an ordered list of intervals.
(defpackage :drom/domain
  (:use :cl :drom/interval)
  (:import-from :utils/misc :else)
  (:export :make-domain
           :domain-union
           :domain-intersection))

(in-package :drom/domain)

(defun make-domain (&rest args)
  (if args
      (mapcar #'ensure-interval args)
      (list make-interval)))

(defun domain-union (d1 d2)
  (reverse (%domain-union d1 d2 nil)))

(defun domain-intersection (d1 d2)
  (reverse (%domain-intersection d1 d2 nil)))

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

(defun %domain-intersection (d1 d2 acc)
  (if (and d1 d2) 
      (let ((a (car d1))
            (b (car d2)))
        (ecase (interval-relation a b)
          (a<b
            (%domain-intersection (cdr d1) d2 acc))
          (a-intersects-b
            (let ((i (cons (car b) (cdr a))))
              (%domain-intersection (cdr d1) d2 (cons i acc))))
          (b-subset-a 
            (%domain-intersection d1 (cdr d2) (cons b acc))) 
          (a-subset-b
            (%domain-intersection (cdr d1) d2 (cons a acc)))
          (b-intersects-a
            (let ((i (cons (car a) (cdr b))))
              (%domain-intersection d1 (cdr d2) (cons i acc))))
          (a>b (%domain-intersection d1 (cdr d2) acc))))
      acc))


