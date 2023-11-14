(defpackage :drom/plot
  (:use :cl :sdl2 :drom/raster)
  (:import-from :utils/misc :on :this :rapprox= :*epsilon* :with-if :then)
  (:import-from :alexandria :curry)
  (:import-from :autowrap :c-aref)
  (:export :plot))

(in-package :drom/plot)

(defmacro with-texture ((name renderer pixel-format access width height) &body body)
  `(let ((,name (sdl2:create-texture ,renderer ,pixel-format ,access ,width ,height)))
     (unwind-protect
          (progn ,@body)
       (sdl2:destroy-texture ,name))))

(defmacro with-texture-lock ((texture pixels &optional rect) &body body)
  `(progn
     (let ((pixels (lock-texture ,texture)))
       (unwind-protect
         (progn ,@body)
         (unlock-texture ,texture)))))

(defun plot (texture function domain resolution)
  "Plot a function over time.
   function --- function time x y"
  (with-texture-lock (texture pixels)
    (raster-map-1
      (lambda (indices value)
        (setf (autowrap:c-aref pixels (+ (first indices) (* 512 (second indices))) :unsigned-int)
              (if value #xffffffff #x00000000))) 
      (curry function (/ (get-ticks) 1000))
      domain
      resolution)))

(defun clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun plot-test ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (with-texture (texture renderer +pixelformat-rgba32+ sdl2-ffi:+sdl-textureaccess-streaming+
                               512 512)
          (sdl2:with-event-loop (:method :poll)
            (:keyup
              (:keysym keysym)
              (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                (sdl2:push-event :quit)))
            (:idle
              ()
              (clear renderer)
              (sdl2:set-render-draw-color renderer 255 255 255 255)
              (plot texture (lambda (time x) (sin (+ x time))) `((0 . ,(* 4 pi)) (-1 . 1)) '(512 512))
              (sdl2:render-copy renderer texture)
              (sdl2:render-present renderer)
              )
            (:quit () t)))))))




