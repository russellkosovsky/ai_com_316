
(define visited '())  ;visited nodes
(define frontier '()) ;frontier nodes
(define stop-count 500) ;stop count

;; track and draw visited and frontier nodes during search
(define track-and-draw
  (lambda (grid)
    (for-each (lambda (node) (draw-pt-visited node)) visited)    
    (for-each (lambda (node) (draw-pt-frontier node)) frontier)))

;; draw visited node
(define draw-pt-visited
  (lambda (pt)
    (draw-visited (car pt) (cadr pt))))

;; draw frontier node
(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; randomize the order of a list
(define randomize
  (lambda (lst)
    (let ((size (length lst)))
      (cond 
        ;; if list has fewer than 2 elements, return it as is
        ((< size 2) lst)
        (else
          ;; pick random element from the list and recursively randomize the rest
          (let ((node-num (random size)))
            (cons (list-ref lst node-num) 
                  (randomize (removex node-num lst)))))))))

;; remove an element at a given position from the list
(define removex
  (lambda (num lst)
    (if (= num 0)
      ;return the rest of the list excluding the first element
      (cdr lst)
      ; keep the first element and recursively call the function on the rest of the list
      (cons (car lst) (removex (- num 1) (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; calculate the heuristic for a list of points
(define calculate-h
  (lambda (lst)
    (map h lst)))

;; calculates blockwise distance from the current point to the goal
(define h
  (lambda (point)
    (+ (abs (- (car point) (car goal)))  ;; horizontal distance
       (abs (- (cadr point) (cadr goal))))))  ;; vertical distance

;; get the next best move for the robot
; (define get-next-robot 
;   (lambda (point)
;     (let* ((lst1 (cons point (adjacento point)))  ;; get adjacent points of the current position
;            ;(lst0 (randomize lst1))  ;; randomize the list of adjacent points
;            ;(flst (calculate-h lst0))  ;; calc the heuristic values for adjacent points
;            (flst (calculate-h lst1))  ;; calc the heuristic values for adjacent points
;            ;(lst (map list flst lst0)))  ;; create a list pairing heuristic values with the points
;            (lst (map list flst lst1)))  ;; create a list pairing heuristic values with the points
;       (set! queue '()) ;; clear the queue
;       (enqueue lst)    ;; enqueue the new list
;       (let (;(num (random 10))  ;; random number
;             ;(len (length lst0));; length of the randomized list
;             (best (front)))    ;; get the best move from the front of the queue
;          ;(cond 
;            ;((= num 0) (list-ref lst0 (random len))) ;; sometimes return a random move for exploration
;            ;(else best))))) ;; otherwise return the best move
;            (best))))) ;; return the best move
(define get-next-robot 
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))  ;; get adjacent points of the current position
           ;(lst0 (randomize lst1))  ;; randomize the list of adjacent points
           ;(flst (calculate-h lst0))  ;; calc the heuristic values for adjacent points
           (flst (calculate-h lst1))  ;; calc the heuristic values for adjacent points
           ;(lst (map list flst lst0)))  ;; create a list pairing heuristic values with the points
           (lst (map list flst lst1)))  ;; create a list pairing heuristic values with the points
      (set! queue '()) ;; clear the queue
      (enqueue lst)    ;; enqueue the new list
      ;return the best move (front of the queue)
      (front))))


;; move the robot based on the next best point
(define search-robot
  (lambda (grid)
    (let ((next-robot (get-next-robot robot))) ;; get the next move for the robot
      (set! visited (cons robot visited)) ;; add current node to visited
      (set! frontier (adjacento robot)) ;; update frontier with adjacent nodes
      (track-and-draw grid) ;; draw the visited and frontier nodes
      (set! robot next-robot) ;; move the robot
      (if (not (null? robot))
        (draw-moved-robotx robot)))))

;; main search function that starts the search process
(define search
  (lambda (grid stop-count)
    (search2 grid 1 stop-count)))  ;; begin search with a count of 1

;; recursive search function that continues until the goal is reached or stop-count is exceeded
(define search2
  (lambda (grid count stop-count)
    (display count)
    (newline)
    (cond 
      ((equal? robot goal)
        (display "Robot attains the goal"))
      ((>= count stop-count)
        (display "Took too long")
        (newline))
      (else ;; otherwise continue searching
        (pause pause-num)  ;; delay
        (search-robot grid)  ;; move the robot to the next position
        (if (null? robot)
          (display "Cannot reach the goal")
          (begin
            (pause pause-num)
            (search2 grid (+ count 1) stop-count)))))))


