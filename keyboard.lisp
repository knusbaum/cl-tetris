(in-package :cl-tetris)

;;;; Keyboard
(defclass keyboard ()
  ((keys
    :initform (make-hash-table)
    :accessor keys)))

(defun keydown-keysym (kb ks)
  (let ((curr-val (gethash (sdl2:scancode ks) (keys kb))))
    (if (not curr-val)
        (setf (gethash (sdl2:scancode ks) (keys kb))
              :pressed))))

(defun keyup-keysym (kb ks)
  (setf (gethash (sdl2:scancode ks) (keys kb)) nil))

(defun is-key-down (kb sym)
  (let ((curr-val (gethash sym (keys kb))))
    (or (eq curr-val :pressed) (eq curr-val :pressed-and-checked))))

(defun is-key-pressed (kb sym)
  (let ((curr-val (gethash sym (keys kb))))
    (when (eq curr-val :pressed)
      (setf (gethash sym (keys kb)) :pressed-and-checked)
      t)))
