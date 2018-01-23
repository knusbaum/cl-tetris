(defpackage :cl-tetris
  (:use :cl)
  (:export tetris))

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
(defmacro with-full-sdl-init ((width height) &rest body)
  `(sdl2:with-init (sdl2-ffi:+sdl-init-video+ sdl2-ffi:+sdl-init-events+)
     (sdl2:with-window (*window* :w ,width :h ,height :flags '(:shown))
       (sdl2:with-renderer (*renderer* *window* :flags '(:accelerated))
         (unwind-protect
              (let ((*screen-width* width)
                    (*screen-height* height))
                (sdl2:set-render-draw-color *renderer* 0 0 0 #xFF)
                (sdl2:set-render-draw-blend-mode *renderer* sdl2-ffi:+sdl-blendmode-blend+)
                ,@body))))))

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
      (setf (fps fc) (/ 1000 ticks-per-frame))
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

(defun draw-grid ()
  (sdl2:set-render-draw-color *renderer* 0 0 0 25)

  (loop for i from 0 below *screen-width* by *block-size-px*
     do (sdl2:render-draw-line *renderer* i 0 i *screen-height*))

  (loop for i from 0 below *screen-height* by *block-size-px*
     do (sdl2:render-draw-line *renderer* 0 i *screen-width* i)))

(defun make-tetris-array ()
  (make-array (list (truncate *screen-width* *block-size-px*)
                    (truncate *screen-height* *block-size-px*))
              :initial-element nil))

(defun complete-row (ta i)
  (loop for j from 0 below (car (array-dimensions ta))
     when (not (aref ta j i))
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

(defun random-block ()
  (let ((blocknum (random 7))
        (x-pos 5)
        (y-pos 0))

    (case blocknum
      (0 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t t t))
                        :color (make-instance 'color
                                              :red 0
                                              :green 255
                                              :blue 255)))
      (1 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t t)
                                (nil nil t))
                        :color (make-instance 'color
                                              :red 0
                                              :green 0
                                              :blue 255)))
      (2 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t t)
                                (t nil nil))
                        :color (make-instance 'color
                                              :red 255
                                              :green 165
                                              :blue 0)))
      (3 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t)
                                (t t))
                        :color (make-instance 'color
                                              :red 255
                                              :green 255
                                              :blue 0)))
      (4 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((nil t t)
                                (t t nil))
                        :color (make-instance 'color
                                              :red 0
                                              :green 255
                                              :blue 0)))
      (5 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t t)
                                (nil t nil))
                        :color (make-instance 'color
                                              :red 255
                                              :green 0
                                              :blue 255)))
      (6 (make-instance 'block-seq
                        :x-pos x-pos
                        :y-pos y-pos
                        :rows '((t t nil)
                                (nil t t))
                        :color (make-instance 'color
                                              :red 255
                                              :green 0
                                              :blue 0))))))


(defun draw-block-seq (bs)
  (sdl2:set-render-draw-color *renderer*
                              (red (color bs)) (green (color bs)) (blue (color bs)) 255)
  (loop
     for row in (rows bs)
     for row-i from 0
     do (loop
           for col in row
           for col-i from 0
           when col
           do (sdl2:with-rects ((dest (+ (* *block-size-px* (x-pos bs))
                                         (* *block-size-px* col-i))
                                      (+ (* *block-size-px* (y-pos bs))
                                         (* *block-size-px* row-i))
                                      *block-size-px*
                                      *block-size-px*))
                (sdl2:render-fill-rect *renderer* dest)))))

(defun move-down (bs ta)
  (incf (y-pos bs)))

(defun move-left (bs ta)
  (incf (x-pos bs) -1)
  (when (block-collides bs ta)
    (incf (x-pos bs))))

(defun move-right (bs ta)
  (incf (x-pos bs))
  (when (block-collides bs ta)
    (incf (x-pos bs) -1)))

(defun rotate-block (bs ta)
  (let ((orig-width (length (rows bs)))
        (new-width (length (car (rows bs))))
        (old-rows (rows bs)))

    (setf (rows bs)
          (loop
             for i from 0 below (length (car (rows bs)))
             collect (loop for j from (1- (length (rows bs))) downto 0
                        collect (elt (elt (rows bs) j) i))))

    (when (not (= orig-width new-width))
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
      (progn
        (loop
           for row in (rows bs)
           for row-i from 0
           for y-position = (+ row-i (y-pos bs))
           when (>= y-position 0)
           do (loop
                 for col in row
                 for col-i from 0
                 when col
                 do (when (aref tetris-array (+ col-i (x-pos bs)) y-position)
                      (return-from block-collides t))))
        nil)))

(defun eliminate-row (bs row)
  (when (and
         (>= row (y-pos bs))
         (< row (+ (y-pos bs) (length (rows bs)))))
    (let ((row-to-delete (- row (y-pos bs))))
      (if (= row-to-delete 0)
          (setf (rows bs) (cdr (rows bs)))
          (setf (rows bs)
                (loop for elem in (rows bs)
                   for i from 0
                   when (not (= i row-to-delete))
                   collect elem)))))
  (if (>= row (y-pos bs))
      t
      nil))

(defun eliminate-rows (bss row)
  (loop for bs in bss
     when (eliminate-row bs row)
     collect bs))

(defun commit-block (bs tetris-array)
  (loop
     for row in (rows bs)
     for row-i from 0
     do (loop
           for col in row
           for col-i from 0
           when col
           do (setf (aref tetris-array (+ (x-pos bs) col-i) (+ (y-pos bs) row-i)) t))))



(defun tetris ()
  (let ((width 400)
        (height 800))
    (with-full-sdl-init (width height)
      (let ((fc (make-instance 'framerate-manager
                               :frame-duration-ms 10))
            (bs (random-block))
            (bss nil)
            (ta (make-tetris-array))
            (drop-frames 100)
            (background-color 0)
            (blocks-to-move-down nil)
            (speedup-frames 6000))
        (sdl2:with-event-loop (:method :poll :background nil)
          (:idle ()
                 (begin-frame fc)

                 (sdl2:set-render-draw-color *renderer* 0 0 0 255)
                 (sdl2:render-clear *renderer*)

                 (loop for i from 0 below height
                    for red from 0 by 0.1
                    do (progn
                         (sdl2:set-render-draw-color *renderer*
                                                     (truncate red)
                                                     background-color
                                                     background-color
                                                     255)
                         (sdl2:render-draw-line *renderer* 0 i width i)))

                 (when (> background-color 0)
                   (incf background-color -1))

                 (when (= 0 (mod (total-frames fc) speedup-frames))
                   (setf drop-frames (truncate drop-frames 1.1)))

                 (when (= 0 (mod (total-frames fc) drop-frames))
                   (move-down bs ta)
                   (when (block-collides bs ta)
                     (setf background-color 100)
                     (incf (y-pos bs) -1)
                     (commit-block bs ta)
                     (push bs bss)
                     (setf bs (random-block))
                     (when (block-collides bs ta)
                       (sdl2:push-event :quit))))



                 (draw-block-seq bs)
                 (loop for block in bss
                    do (draw-block-seq block))

                 (draw-grid)

                 (when blocks-to-move-down
                   (loop for bs in blocks-to-move-down
                      do (move-down bs ta))
                   (setf blocks-to-move-down nil))

                 (let ((complete (complete-rows ta)))
                   (when complete
                     (let ((start-row (car complete)))
                       (loop for row from start-row downto 0
                          do
                            (loop for col from 0 below (car (array-dimensions ta))
                               do (setf (aref ta col row)
                                        (if (> row 0)
                                            (aref ta col (1- row))
                                            nil))))
                       (setf blocks-to-move-down
                             (eliminate-rows bss start-row)))

                     (setf bss (remove-if (lambda (bs) (not (rows bs))) bss))))

                 (sdl2:render-present *renderer*)
                 (sdl2:delay (calculate-delay fc))
                 (end-frame fc)
                 (calculate-fps fc))

          (:keydown (:keysym keysym)
                    (when (eq (sdl2:scancode keysym) :scancode-q)
                      (sdl2:push-event :quit))

                    (when (eq (sdl2:scancode keysym) :scancode-right)
                      (move-right bs ta))

                    (when (eq (sdl2:scancode keysym) :scancode-left)
                      (move-left bs ta))

                    (when (eq (sdl2:scancode keysym) :scancode-up)
                      (rotate-block bs ta))

                    (when (eq (sdl2:scancode keysym) :scancode-space)
                      (rotate-block bs ta))

                    (when (and (eq (sdl2:scancode keysym) :scancode-down)
                               (not (block-collides bs ta)))
                      (move-down bs ta)
                      (when (block-collides bs ta)
                        (incf (y-pos bs) -1))))
          (:quit ()
                 t))))))
