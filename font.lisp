(in-package :cl-tetris)

(defvar *char-cache* nil
  "Holds a cache of character -> texture.")

(defvar *font* nil
  "Holds the initialized font.")

(defun init-text-drawing (ttf-path size)
  (setf *font* (sdl2-ttf:open-font ttf-path size))
  (setf *char-cache* (make-array 256 :initial-element nil)))

(defun font-texture-for (c)
  (declare (type (simple-vector *) *char-cache*))
  (let ((tex (elt *char-cache* (char-code c))))
    (or tex
        (let* ((surface
                (sdl2-ttf:render-text-solid *font* (string c) #xFF #xFF #xFF #xFF))
               (texture
                (sdl2:create-texture-from-surface *renderer* surface)))
          (format t "Creating new texture for ~a~%" c)
          (setf (elt *char-cache* (char-code c)) texture)
          texture))))

(defun draw-string (destination &rest strings)
  (let ((dest-start-x (sdl2:rect-x destination)))
    (sdl2:with-rects ((src-rect 0 0 0 0))
      (loop for s in strings
         do
           (loop
              with dest-rect = destination
              for c across s
              as char-tex = (font-texture-for c)
              do (if (eq c #\newline)
                     (progn
                       (incf (sdl2:rect-y dest-rect) (+ 5 (sdl2:texture-height char-tex)))
                       (setf (sdl2:rect-x dest-rect) dest-start-x))
                     (let ((c-width (sdl2:texture-width char-tex))
                           (c-height (sdl2:texture-height char-tex)))
                       (setf (sdl2:rect-width src-rect) c-width)
                       (setf (sdl2:rect-height src-rect) c-height)

                       (setf (sdl2:rect-width dest-rect) c-width)
                       (setf (sdl2:rect-height dest-rect) c-height)
                       (sdl2:render-copy *renderer* char-tex
                                    :source-rect src-rect
                                    :dest-rect dest-rect)

                       (incf (sdl2:rect-x dest-rect) (sdl2:texture-width char-tex)))))))))
