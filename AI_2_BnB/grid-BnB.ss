
(define path-lst '())  ;store the child-parent relationship for each point
(define goal -1)  ;default value
(define visited 1) ; mark points as visited

;; find adjacent points and add them to the queue
(define expand
  (lambda (point)
    (let ((lst (adjacentv point)))  ; get list of adjacent points
      (set-lst-visited lst)  ; mark all adjacent points as visited
      (add-to-path-lst lst point)  ; store parent-child relationships
      (enqueue lst))))  ; add adjacent points to the queue

;; update path-lst (recording the child-parent relationship)
(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))  ;if the list is not empty
        (let ((child-parent (list (car lst) point)))  ;make a pair of current node and its parent
          (set! path-lst (cons child-parent path-lst))  ;add pair to path-lst
          (add-to-path-lst (cdr lst) point)))))  ;recursively add remaining points

;; mark the points in the list as visited
(define set-lst-visited
  (lambda (lst)
    (if (null? lst)  ; if list empty -- return an empty list
        '()
    ;else mark each point in the list as visited
        (let ((x (car lst)))  ;get current point
          (draw-pt-frontier x)  ;draw current point as frontier
          (block-set! x visited)  ;mark point visited on grid
          (set-lst-visited (cdr lst))))))  ;recursively mark rest of the points as visited

;; draw a frontier point by coordinates
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))  ;draw point on grid

;; starts the search and initializes the path-lst
(define search
  (lambda (grid stop-count)
    (block-set! start visited)  ;mark start point as visited
    (set! path-lst (list (list start '())))  ;init path-lst with start point
    (search2 grid 1 stop-count)))  ;call recursive search function

;; recursive function that explores the grid
(define search2
  (lambda (grid count stop-count)
    (pause pause-num)  ;pause to allow the viz
    (expand robot)  ;expand current robot position
    (set! queue (list-sort heuristic_compare queue))  ;sort the queue based on the heuristic
    (draw-moved-robot (robot-x) (robot-y))  ;update robot position on the grid
    (draw-visited (car robot) (cadr robot))  ;mark robot's current position as visited
    (let ((next-robot (front)))  ;get next robot position from the front of the queue
      (cond
        [(>= count stop-count) #f]  ;stop search if stop count is reached
        [(equal? next-robot goal)  ;check if the next position is the goal
         (draw-path (get-path next-robot))  ;draw the path to the goal
         (draw-moved-robot (car next-robot) (cadr next-robot))  ;move robot to the goal
         (get-path next-robot)]  ;return the path
        [else  ;continue search
         (dequeue)  ;remove the front element from the queue
         (set! robot next-robot)  ;update the robot position
         (search2 grid (+ count 1) stop-count)]))))  ;recursive call to continue the search

;; calculate the distance between two points
(define distance
  (lambda (node1 node2)
    (+ (abs (- (car node1) (car node2)))  ;absolute difference in x coordinates
       (abs (- (cadr node1) (cadr node2)))))  ;absolute difference in y coordinates
    )

;; calculates total cost by adding the current path length and the distance to the goal
(define heuristic
  (lambda (number point)
    (+ number (distance point goal))))  ;path length plus the distance to the goal

;; compare two nodes based on their heuristic value
(define heuristic_compare
  (lambda (node1 node2)
    (< (heuristic (length (get-path node1)) node1)
       (heuristic (length (get-path node2)) node2))))

;; draw the entire path from start to goal
(define draw-path
  (lambda (path)
    (cond
      ((not (null? path))  ;if path not empty
       (draw-pt-path-node (car path))  ;draw current point
       (draw-path (cdr path))))))  ;recursively draw the rest

;; get the path by following parent-child relationships from goal back to start
(define get-path
  (lambda (last-node)
    (cond
      [(equal? start last-node) (list start)]  ;if last node is start -- return the start
      [else (cons last-node (get-path (cadr (assoc last-node path-lst))))])))  ;recursively follow path back to start

;; draw each point in the path
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))  ;draw point based on its coordinates
