;; initialize an empty list to store the path and set goal and visited to default values
(define path-lst '())  ; path-lst will store the child-parent relationship for each point
(define goal -1)  ; default value for the goal
(define visited 1) ; visited is used to mark points as visited

;; expand the current point by finding its adjacent points and adding them to the queue
(define expand
  (lambda (point)
    (let ((lst (adjacentv point)))  ; get the list of adjacent points to the current point
      (set-lst-visited lst)  ; mark all adjacent points as visited
      (add-to-path-lst lst point)  ; store the parent-child relationships for path tracking
      (enqueue lst))))  ; add all adjacent points to the queue

;; function to update the path-lst by recording the child-parent relationship
(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))  ; check if the list is not empty
        (let ((child-parent (list (car lst) point)))  ; create a pair of the current node and its parent
          (set! path-lst (cons child-parent path-lst))  ; add the pair to the path-lst
          (add-to-path-lst (cdr lst) point)))))  ; recursively add remaining points

;; function to mark the points in the list as visited
(define set-lst-visited
  (lambda (lst)
    (if (null? lst)  ; if the list is empty, return an empty list
        '()
    ;; else mark each point in the list as visited
        (let ((x (car lst)))  ; get the current point
          (draw-pt-frontier x)  ; draw the current point as part of the frontier
          (block-set! x visited)  ; mark the point as visited in the grid
          (set-lst-visited (cdr lst))))))  ; recursively mark the rest of the points as visited

;; function to draw a frontier point based on its coordinates
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))  ; draw the point on the grid

;; search function starts the search and initializes the path-lst
(define search
  (lambda (grid stop-count)
    (block-set! start visited)  ; mark the start point as visited
    (set! path-lst (list (list start '())))  ; initialize the path-lst with the start point
    (search2 grid 1 stop-count)))  ; call the recursive search function

;; recursive search function that explores the grid
(define search2
  (lambda (grid count stop-count)
    (pause pause-num)  ; pause to allow the visualization to update
    (expand robot)  ; expand the current robot position
    (set! queue (list-sort heuristic_compare queue))  ; sort the queue based on the heuristic
    (draw-moved-robot (robot-x) (robot-y))  ; update the robot position on the grid
    (draw-visited (car robot) (cadr robot))  ; mark the robot's current position as visited
    (let ((next-robot (front)))  ; get the next robot position from the front of the queue
      (cond
        [(>= count stop-count) #f]  ; stop the search if the stop count is reached
        [(equal? next-robot goal)  ; check if the next robot position is the goal
         (draw-path (get-path next-robot))  ; draw the path to the goal
         (draw-moved-robot (car next-robot) (cadr next-robot))  ; move the robot to the goal
         (get-path next-robot)]  ; return the path
        [else  ; continue the search
         (dequeue)  ; remove the front element from the queue
         (set! robot next-robot)  ; update the robot position
         (search2 grid (+ count 1) stop-count)]))))  ; recursive call to continue the search

;; function to calculate the distance between two points using the manhattan distance
(define distance
  (lambda (node1 node2)
    (+ (abs (- (car node1) (car node2)))  ; calculate the absolute difference in x coordinates
       (abs (- (cadr node1) (cadr node2)))))  ; calculate the absolute difference in y coordinates
    )

;; heuristic function that calculates the total cost by adding the current path length and the distance to the goal
(define heuristic
  (lambda (number point)
    (+ number (distance point goal))))  ; total cost is the path length plus the distance to the goal

;; function to compare two nodes based on their heuristic value
(define heuristic_compare
  (lambda (node1 node2)
    (< (heuristic (length (get-path node1)) node1)  ; compare the heuristic of the first node
       (heuristic (length (get-path node2)) node2))))  ; compare the heuristic of the second node

;; function to draw the entire path from start to goal
(define draw-path
  (lambda (path)
    (cond
      ((not (null? path))  ; if the path is not empty
       (draw-pt-path-node (car path))  ; draw the current point in the path
       (draw-path (cdr path))))))  ; recursively draw the rest of the path

;; function to get the path by following parent-child relationships from the goal back to the start
(define get-path
  (lambda (last-node)
    (cond
      [(equal? start last-node) (list start)]  ; if the last node is the start, return the start
      [else (cons last-node (get-path (cadr (assoc last-node path-lst))))])))  ; recursively follow the path back to the start

;; function to draw each point in the path
(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))  ; draw the point based on its coordinates
