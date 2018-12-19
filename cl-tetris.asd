

(asdf:defsystem #:cl-tetris
  :name "cl-tetris"
  :description "The game of Tetris"
  :license "MIT"
  :author "Kyle Nusbaum"
  :depends-on (#:trivial-garbage #:sdl2 #:sdl2-ttf)
  :components ((:file "cl-tetris-package")
               (:file "common"
                      :depends-on ("cl-tetris-package"))
               (:file "keyboard"
                      :depends-on ("cl-tetris-package" "common"))
               (:file "font"
                      :depends-on ("cl-tetris-package" "common"))
               (:file "cl-tetris"
                      :depends-on ("cl-tetris-package" "common" "keyboard" "font"))))
