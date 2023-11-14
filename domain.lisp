;;; A domain is represented by an ordered list of intervals.
;;; An interval is a cons of the minimum and maximum values.

;;; TODO: Sort out open/closed intervals.

(defpackage :drom/domain
  (:use :cl :utils/misc :float-features)
  (:export :make-domain
           :make-interval
           :lower
           :upper
           :domain-union
           :domain-intersection))

(in-package :drom/domain)

(defun make-interval (&key lower upper)
  (cons (else lower single-float-negative-infinity)
        (else upper single-float-positive-infinity)))

(defun lower (interval) (car interval))
(defun upper (interval) (cdr interval))

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

(defun map-interval (f &rest intervals)
  (let ((lower-bounds (mapcar #'lower intervals))
        (upper-bounds (mapcar #'upper intervals))))
  (make-interval :lower (apply f lower-bounds)
                 :upper (apply f upper-bounds)))

(defun make-domain (&rest intervals)
  (else intervals `(,(make-interval))))

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


