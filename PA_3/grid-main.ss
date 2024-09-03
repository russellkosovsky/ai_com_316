; grid-main.ss


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Modify the program so that it marks visited nodes (squares) as visited.  

;Then modify the program so that it will choose a free node over a visited 
;  if it has a choice.  

;For bothof these modifications you only need to change/add-to grid-search.ss.

;Although changing the value of pause-num in grid-main.ss will help
;  you see what's going on by reducing the speed.  

;Try changing other valuesin this file to see the effects.  

;Run the the original program and your modified program each 10 times.  
;  Does your modification improve the steps taken to find the goal? 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define num-col-row 20)                   ; grid size
(define pause-num 100000)                 ; speed of agent
(define size (floor (/ 500 num-col-row))) ; window size
(define obstacle-density 30)

(load "grid-class.ss")
(load "grid-draw.ss")
(load "grid-make.ss")
(load "grid-search.ss")

(define grid0 (make-grid num-col-row)) 

(draw-obstacles grid0)

(define grid (convert-grid grid0))

(set-goal grid)
(set-start grid)
(draw-start)
(draw-goal)
(draw-robot)
(show canvas)
(search grid 20000)

