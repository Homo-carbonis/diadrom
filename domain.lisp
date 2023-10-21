;;; A domain is represented by a simple value, an interval, or an ordered list of intervals.
;;; An interval is a cons of the minimum and maximum values.
(defpackage :dyn/domain
  (:use :cl :iterate))

(defun domain-union (d1 d2)
  (on nil
    (iter (for i1 in d1)
          (iter (for i2 in d2)
                        ))))

(defun interval-union (a b)
  (ecase (interval-relation a b)
    ('a-intersects-b (cons (car a) (cdr b)))
    ('b-subset-a a)
    ('a-subset-b b)
    ('b-intersects-a (cons (car b) (cdr a)))
    ('disjoint) nil))

(defun interval-relation (a b)
  (if (< (car a)) (car b)
    (if (< (cdr a) (car b))
        'disjoint
        (if (< (cdr a) (cdr b))
            'a-intersects-b
            'b-subset-a)
        (if (< (car a) (cdr b))
            (if (< (cdr a) (cdr b))
                'a-subset-b
                'b-intersects-a)
            'disjoint))))

(defun in-interval (x interval)
  (and (> x (car interval))
       (< x (cdr interval))))
