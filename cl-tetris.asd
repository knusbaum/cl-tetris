

(asdf:defsystem #:cl-tetris
  :name "cl-tetris"
  :description "The game of Tetris"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:sdl2)
  :components ((:file "cl-tetris")))
