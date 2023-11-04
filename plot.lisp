(defpackage :drom/plot
  (:use :cl :sdl2 :drom/raster)
  (:import-from :utils/misc :on :this :approx=)
  (:import-from :alexandria :curry)
  (:export :plot))

(in-package :drom/plot)

(defun plot (renderer function domain resolution)
  (raster-map (lambda (coords value)
                (when value
                  (apply #'render-draw-point renderer coords)))
              (curry function (/(get-ticks) 1000))
              domain
              resolution))



(defun clear (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer))

(defun plot-test ()
  "Test the SDL_render.h API"
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "SDL2 Renderer API Demo" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)
          (:keyup
           (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))
          (:idle
           ()
           (clear renderer)
           (sdl2:set-render-draw-color renderer 255 255 255 255)
           (plot renderer (lambda (time x y) (approx= y (sin (+ x time)) :epsilon 0.01)) '((0 . 12) (-1 . 1)) '(1000 500))
           (sdl2:render-present renderer)
	   (sdl2:delay 33))
          (:quit () t))))))
