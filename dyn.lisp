; Examples
; (def-component cell () ((point)) (eq (distance (nearest cell)) 1))
; (cell)
; (alias-component point (dimension v))
; (defun distance (a b) (magnitude (- (point b) (point a))))
; (defun nearest (a) (min (amap (distance a a1) (family a))))  

; Example component lists
; (5 point)
; ((point) r)


(in-package #:dyn)
(named-readtables:in-readtable lol:lol-syntax)

(defmacro defsys (name lambda-list subsystems &body rules)
  (let* ((key-vars (lambda-list->alist lambda-list))
        (vars (mapcar #'cdr key-vars)))
    `(defmacro ,name ,lambda-list
       (let ((rules
               (append
; vars are bound in the top level macro so we use ,a1 to access the name of the
; variable and ,(eval a1) to access its value.
                (mapcar #`(= ,a1 ,(eval a1)) ',vars)
                ',rules)))
         (let ,subsystems
           `(lambda (key)
              (let ((var (assoc key ,',key-vars)))
                (solve var ,@rules))))))))

(defun lambda-list->alist (lambda-list)
  "Produce a alist of keys and variables from an ordinary lambda list."
  (mvbind (required optional rest key allow-other-keys aux)
    (parse-ordinary-lambda-list lambda-list)
    (append (mapcar (lambda (x) (cons (make-keyword x) x)) required)
            (mapcar (lambda (x) (cons (make-keyword (car x)) (car x))) optional)
            (mapcar (lambda (x) (cons (make-keyword (caar x)) (cadar x))) key))))

(defparameter *depth-limit* 10)
