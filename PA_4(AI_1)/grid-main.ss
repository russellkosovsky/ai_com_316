<<<<<<< HEAD:PA_4/grid-main.ss.backup
(define num-col-row 10)
(define pause-num 1000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 10)

(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-stack.ss")
(load "grid-queue.ss")

(define grid0 (make-grid num-col-row)) 
(draw-obstacles grid0)
(define grid (convert-grid grid0))

(load "grid-new.ss")
;(load "grid-BFS.ss")
(load "grid-DFS.ss")

(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
=======
(define num-col-row 10)
(define pause-num 1000)
(define size (floor (/ 700 num-col-row)))
(define obstacle-density 10)

(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-stack.ss")
(load "grid-queue.ss")

(define grid0 (make-grid num-col-row)) 
(draw-obstacles grid0)
(define grid (convert-grid grid0))

(load "grid-new.ss")
;(load "grid-BFS.ss")
(load "grid-DFS.ss")

(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
>>>>>>> b4f7311 (yurt):PA_4(AI_1)/grid-main.ss
(search grid 20000)