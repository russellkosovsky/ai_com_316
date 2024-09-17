(define num-col-row 100)
(define pause-num 1000)
(define size (floor (/ 1000 num-col-row)))
(define obstacle-density 15)

(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-stack.ss")
;(load "grid-queue.ss")
(load "grid-priority-queue.ss")


(define grid0 (make-grid num-col-row)) 
(draw-obstacles grid0)
(define grid (convert-grid grid0))

(load "grid-new.ss")
(load "grid-BnB.ss")


(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search grid 20000)
