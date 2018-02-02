(in-package :cl-tetris)

(defun score-for (n)
  (case n
    (1 100)
    (2 200)
    (3 300)
    (4 400)))

(defun tetris-game ()
  (let ((width 400)
        (height 800))
    (with-full-sdl-init (width height)
      (let ((fr-mgr (make-instance 'framerate-manager
                                   :frame-duration-ms 10))
            (bs (random-block))
            (bss nil)
            (ta (make-tetris-array))
            (drop-frames 100)
            (background-color 0)
            (blocks-to-move-down nil)
            (speedup-frames 3000)
            (keyboard (make-instance 'keyboard))
            (score 0))
        (sdl2:with-event-loop (:method :poll :background nil)
          (:idle ()
                 (begin-frame fr-mgr)

                 ;; keyboard update
                 (when (zerop (mod (total-frames fr-mgr) 2)) ;mod 2 to decrease speed.
                   (when (is-key-down keyboard :scancode-q)
                     (sdl2:push-event :quit))

                   (when (is-key-pressed keyboard :scancode-right)
                     (move-right bs ta))

                   (when (is-key-pressed keyboard :scancode-left)
                     (move-left bs ta))

                   (when (is-key-pressed keyboard :scancode-up)
                     (rotate-block bs ta))

                   (when (is-key-pressed keyboard :scancode-space)
                     (rotate-block bs ta))

                   (when (and (is-key-down keyboard :scancode-down)
                              (not (block-collides bs ta))
                              (zerop (mod (total-frames fr-mgr) 2)))
                     (move-down bs ta)
                     (when (block-collides bs ta)
                       (decf (y-pos bs)))))


                 ;; main update
                 (sdl2:set-render-draw-color *renderer* 0 0 0 255)
                 (sdl2:render-clear *renderer*)

                 (loop for i from 0 below height
                    for red from 0 by 0.1
                    do (sdl2:set-render-draw-color *renderer*
                                                   (truncate red)
                                                   background-color
                                                   background-color
                                                   255)
                      (sdl2:render-draw-line *renderer* 0 i width i))

                 (when (> background-color 0)
                   (decf background-color))

                 (when (zerop (mod (total-frames fr-mgr) speedup-frames))
                   (setf drop-frames (truncate drop-frames 1.1)))

                 (when (zerop (mod (total-frames fr-mgr) drop-frames))
                   (move-down bs ta)
                   (when (block-collides bs ta)
                     (setf background-color 100)
                     (decf (y-pos bs))
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
                   (unless (null complete)
                     (incf score (score-for (length complete)))
                     (let ((start-row (car complete)))
                       (loop for row from start-row downto 0
                          do
                            (loop for col from 0 below (car (array-dimensions ta))
                               do (setf (aref ta col row)
                                        (if (plusp row)
                                            (aref ta col (1- row))
                                            nil))))
                       (setf blocks-to-move-down
                             (eliminate-rows bss start-row)))

                     (setf bss (remove-if (lambda (bs) (null (rows bs))) bss))))


                 (sdl2:with-rects ((text-dest 5 5 0 0))
                   (draw-string text-dest (format nil "Score: ~a" score)))

                 (sdl2:render-present *renderer*)
                 (sdl2:delay (calculate-delay fr-mgr))
                 (end-frame fr-mgr)
                 (calculate-fps fr-mgr))

          (:keydown (:keysym keysym)
                    (keydown-keysym keyboard keysym))
          (:keyup (:keysym keysym)
                  (keyup-keysym keyboard keysym))
          (:quit ()
                 t))
        score))))

(defun display-score (score)
  (let ((width 600)
        (height 100))
    (with-full-sdl-init (width height :font-size 50)
      (let ((fr-mgr (make-instance 'framerate-manager
                                   :frame-duration-ms 10)))
        (sdl2:with-event-loop (:method :poll :background nil)
          (:idle ()
                 (begin-frame fr-mgr)

                 (sdl2:set-render-draw-color *renderer* 0 0 0 255)
                 (sdl2:render-clear *renderer*)

                 (sdl2:with-rects ((text-dest 5 5 0 0))
                   (draw-string text-dest (format nil "FINAL SCORE: ~a" score)))

                 (sdl2:render-present *renderer*)

                 (sdl2:delay (calculate-delay fr-mgr))
                 (end-frame fr-mgr)
                 (calculate-fps fr-mgr))
          (:keydown (:keysym keysym)
                    (when (equal (sdl2:scancode keysym) :scancode-q)
                      (sdl2:push-event :quit)))
          (:quit ()
                 t))))))

;;; The main tetris function
(defun tetris ()
  (let ((score (tetris-game)))
    (display-score score)))
        
