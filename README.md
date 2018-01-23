# CL-TETRIS
---

Tetris written in Common lisp

## Requirements

The only dependencies are cl-sdl2 (this in turn requires libsdl2 to be installed)

### To Run
```
(asdf:load-system 'cl-tetris)
(cl-tetris:tetris)
```

#### Controls:
 * left-arrow: move left
 * right-arrow: move right
 * down-arrow: move down
 * space: rotate
 * Q key: quit
 