(defpackage :drom/raster
  (:use :cl :drom/domain)
  (:import-from :utils/misc :on :this :with-if :then :*epsilon*)
  (:import-from :alexandria :map-product :iota :rcurry :ensure-function)
  (:export :raster-map))

(in-package :drom/raster)

(defun raster-map (map-function function domain resolution)
  (labels ((%raster-map (f domain resolution indices)
             (let* ((interval (car domain))
                    (domain (cdr domain))
                    (lower (lower interval))
                    (upper (upper interval))
                    (n (car resolution))
                    (resolution (cdr resolution))
                    ;; Use dynamic binding of *epsilon* so we have the correct
                    ;; precision in `function`.
                    (*epsilon* (/ (- upper lower) n)))
               (with-if domain
                        (loop for x from lower below upper by *epsilon*
                              for i below n
                              for %indices = (cons i indices)
                              do (then (%raster-map (rcurry f x) domain resolution %indices)
                                       (funcall map-function %indices (funcall f x))))))))
    (%raster-map (ensure-function function) (reverse domain) (reverse resolution) nil)))
