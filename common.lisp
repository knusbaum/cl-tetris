(in-package :cl-tetris)

(defvar *window* nil
  "Dynamically bound to the current window for the game.")
(defvar *renderer* nil
  "Dynamically bound to the current renderer for the game.")

(defvar *screen-width* nil)
(defvar *screen-height* nil)
(defvar *block-size-px* 40)

;;; This initializes everything that needs to be initialized
;;; for the game to work
(defmacro with-full-sdl-init ((width height &key (font-size 18) (title "CL-Tetris")) &rest body)
  `(sdl2:with-init (sdl2-ffi:+sdl-init-video+ sdl2-ffi:+sdl-init-events+)
     (sdl2:with-window (*window* :w ,width :h ,height :flags '(:shown) :title ,title)
       (sdl2:with-renderer (*renderer* *window* :flags '(:accelerated))
         (sdl2-ttf:init)
         (init-text-drawing ,(merge-pathnames "FreeMono.ttf" *compile-file-truename*) ,font-size)
         (unwind-protect
              (let ((*screen-width* width)
                    (*screen-height* height))
                (sdl2:set-render-draw-color *renderer* 0 0 0 #xFF)
                (sdl2:set-render-draw-blend-mode *renderer* sdl2-ffi:+sdl-blendmode-blend+)
                ,@body)
           (sdl2-ttf:quit))))))

(defclass framerate-manager ()
  ((total-frames :initform 0 :accessor total-frames)
   (frame :initform 0 :accessor frame)
   (start-ticks :accessor start-ticks)
   (curr-ticks :initform 0 :accessor curr-ticks)
   (fps :initform 0 :accessor fps)
   (frames-per-calc :initform 10 :accessor frames-per-calc)
   (frame-duration-ms :initform 10 :initarg :frame-duration-ms :accessor frame-duration-ms)))

(defun begin-frame (fc)
  (setf (start-ticks fc) (sdl2:get-ticks)))

(defun end-frame (fc)
  (incf (frame fc))
  (incf (total-frames fc))
  (incf (curr-ticks fc) (- (sdl2:get-ticks) (start-ticks fc))))

(defun calculate-fps (fc)
  (when (= (frame fc) (frames-per-calc fc))
    (let ((ticks-per-frame (/ (curr-ticks fc) (frame fc))))
      (setf (fps fc) (float (/ 1000 ticks-per-frame)))
      (setf (frame fc) 0)
      (setf (curr-ticks fc) 0)))
      ;(format t "FPS: ~a, Avg frame length (ms): ~a~%"
      ;        (float (calculate-fps fc))
      ;        (float ticks-per-frame))))
  (fps fc))

(defun calculate-delay (fc)
  (let* ((ticks (sdl2:get-ticks))
         (ret (- (frame-duration-ms fc) (- ticks (start-ticks fc)))))
    (max 0 ret)))

(defun draw-grid (start-x)
  (sdl2:set-render-draw-color *renderer* 0 0 0 25)
  (loop for i from 0 below *screen-width* by *block-size-px*
        do (sdl2:render-draw-line *renderer* (+ start-x i) 0 (+ start-x i) *screen-height*))
  (loop for i from 0 below *screen-height* by *block-size-px*
        do (sdl2:render-draw-line *renderer* start-x i (+ start-x *screen-width*) i)))

(defun make-tetris-array ()
  (make-array (list (truncate *screen-width* *block-size-px*)
                    (truncate *screen-height* *block-size-px*))
              :initial-element nil))

(defun complete-row (ta i)
  (loop for j from 0 below (car (array-dimensions ta))
        unless (aref ta j i)
        do (return-from complete-row nil))
  t)

(defun complete-rows (ta)
  (let ((colcount (car (array-dimensions ta)))
        (rowcount (cadr (array-dimensions ta))))
    (loop for i from (1- rowcount) downto 0
          when (complete-row ta i)
          collect i)))

(defclass color ()
  ((red :initarg :red :accessor red)
   (green :initarg :green :accessor green)
   (blue :initarg :blue :accessor blue)))

(defclass block-seq ()
  ((x-pos :initarg :x-pos :accessor x-pos)
   (y-pos :initarg :y-pos :accessor y-pos)
   (rows :initarg :rows :accessor rows)
   (color :initarg :color :accessor color)))

(defun make-block (&key x-pos y-pos rows red green blue)
  (make-instance 'block-seq
                 :x-pos x-pos
                 :y-pos y-pos
                 :rows rows
                 :color (make-instance 'color
                                       :red red
                                       :green green
                                       :blue blue)))
(defun random-block ()
  (let ((blocknum (random 7))
        (x-pos 5)
        (y-pos 0))
    (case blocknum
      (0 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t t t))
                     :red 0 :green 255 :blue 255))

      (1 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t t)
                             (nil nil t))
                     :red 0 :green 0 :blue 255))

      (2 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t t)
                             (t nil nil))
                     :red 255 :green 165 :blue 0))

      (3 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t)
                             (t t))
                     :red 255 :green 255 :blue 0))

      (4 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((nil t t)
                             (t t nil))
                     :red 0 :green 255 :blue 0))

      (5 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t t)
                             (nil t nil))
                     :red 255 :green 0 :blue 255))

      (6 (make-block :x-pos x-pos :y-pos y-pos
                     :rows '((t t nil)
                             (nil t t))
                     :red 255 :green 0 :blue 0)))))

(defun draw-block-seq (bs start-x start-y)
  (draw-block-seq-at bs
                     (+ start-x (* *block-size-px* (x-pos bs)))
                     (+ start-y (* *block-size-px* (y-pos bs)))))

(defun draw-block-seq-at (bs x y)
  (sdl2:set-render-draw-color *renderer*
                              (red (color bs)) (green (color bs)) (blue (color bs)) 255)
  (loop for row in (rows bs)
        for row-i from 0
        do (loop for col in row
                 for col-i from 0
                 when col
                 do (sdl2:with-rects ((dest (+ (* *block-size-px* col-i)
                                               x)
                                            (+ (* *block-size-px* row-i)
                                               y)
                                            *block-size-px*
                                            *block-size-px*))
                                     (sdl2:render-fill-rect *renderer* dest)))))

(defun move-down (bs)
  (incf (y-pos bs)))

(defun move-left (bs ta)
  (decf (x-pos bs))
  (when (block-collides bs ta)
    (incf (x-pos bs))))

(defun move-right (bs ta)
  (incf (x-pos bs))
  (when (block-collides bs ta)
    (decf (x-pos bs))))

(defun rotate-block (bs ta)
  (let ((orig-width (length (rows bs)))
        (new-width (length (car (rows bs))))
        (old-rows (rows bs)))
    (setf (rows bs)
          (loop for i from 0 below (length (car (rows bs)))
                collect (loop for j from (1- (length (rows bs))) downto 0
                              collect (elt (elt (rows bs) j) i))))

    (unless (= orig-width new-width)
      (incf (x-pos bs) (truncate (- new-width orig-width) 2)))

    (incf (y-pos bs) (- orig-width new-width))

    (when (block-collides bs ta)
      (setf (rows bs) old-rows)
      (incf (x-pos bs) (- (truncate (- new-width orig-width) 2)))
      (incf (y-pos bs) (- (- orig-width new-width))))))

(defun block-collides (bs tetris-array)
  (if (or
       (> (+ (y-pos bs) (length (rows bs)))
          (truncate *screen-height* *block-size-px*))
       (> (+ (x-pos bs) (length (car (rows bs))))
          (truncate *screen-width* *block-size-px*))
       (< (x-pos bs) 0))

      (return-from block-collides t)
      (loop for row in (rows bs)
            for row-i from 0
            for y-position = (+ row-i (y-pos bs))
            when (>= y-position 0)
            do (loop for col in row
                     for col-i from 0
                     when col
                     do (when (aref tetris-array (+ col-i (x-pos bs)) y-position)
                          (return-from block-collides t))))))

(defun eliminate-row (bs row)
  (when (and
         (>= row (y-pos bs))
         (< row (+ (y-pos bs) (length (rows bs)))))
    (let ((row-to-delete (- row (y-pos bs))))
      (if (zerop row-to-delete)
          (pop (rows bs))
          (setf (rows bs)
                (loop for elem in (rows bs)
                      for i from 0
                      unless (= i row-to-delete)
                      collect elem)))))
  (>= row (y-pos bs)))

(defun eliminate-rows (bss row)
  (loop for bs in bss
        when (eliminate-row bs row)
        collect bs))

(defun commit-block (bs tetris-array)
  (loop for row in (rows bs)
        for row-i from 0
        do (loop for col in row
                 for col-i from 0
                 when col
                 do (setf (aref tetris-array (+ (x-pos bs) col-i) (+ (y-pos bs) row-i)) t))))
