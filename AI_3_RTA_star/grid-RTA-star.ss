(define path-lst '())
(define goal -1)
(define prev-position '())  ;; track the previous position
(define bad-moves '())      ;; track all "bad" moves that led to getting stuck

(define expand
  (lambda (point)
    (let ((lst (adjacentv point)))
      (set-lst-visited lst)
      (add-to-path-lst lst point)
      (enqueue lst))))

(define add-to-path-lst
  (lambda (lst point)
    (if (not (null? lst))
        (let ((child-parent (list (car lst) point)))
          (set! path-lst (cons child-parent path-lst))
          (add-to-path-lst (cdr lst) point)))))

(define set-lst-visited
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((x (car lst)))
          (draw-pt-frontier x)
          (block-set! x visited)
          (set-lst-visited (cdr lst))))))

(define draw-pt-frontier
  (lambda (pt)
    (draw-frontier (car pt) (cadr pt))))

;; The search function is updated to clear the queue after each move
(define search
  (lambda (grid stop-count)
    (block-set! start visited)
    (set! path-lst (list (list start '())))
    (set! prev-position '())  ;; Initialize previous position
    (set! bad-moves '())  ;; Initialize bad moves list
    (search2 grid 1 stop-count)))

(define search2
  (lambda (grid count stop-count)
    (display count)
    (newline)
    (pause pause-num)
    ;; Clear the queue before expanding, so the robot only "sees" adjacent nodes
    (set! queue '()) ;; Clear the queue
    (expand robot)
    ;; Sort the queue only if it's not empty
    (if (not (null? queue))
        (set! queue (list-sort heuristic_compare queue)))
    (draw-moved-robot (robot-x) (robot-y))   
    (draw-visited (car robot) (cadr robot))
    ;; If the queue is empty, backtrack by revisiting previous nodes
    (if (null? queue)
        (expand robot))
    ;; Proceed with normal move selection
    (let* ((next-robot (if (not (null? queue)) (front) robot))
           (next-move (front)))
      (cond
        [(>= count stop-count) #f]
        [(equal? next-robot goal) 
         (draw-path (get-path next-robot)) 
         (draw-moved-robot (car next-robot) (cadr next-robot)) 
         (get-path next-robot)]
        ;; If the robot revisits a node, avoid "bad moves"
        [(and (equal? next-robot prev-position) 
              (member next-robot bad-moves))
         ;; Remove all bad moves from the queue
         (set! queue (filter (lambda (move) (not (member move bad-moves))) queue))
         ;; Fix: Use a `begin` block to handle multiple expressions
         (if (null? queue)
             (expand robot)
             (begin
               (set! next-move (front))
               (dequeue)
               (set! robot next-move)
               (search2 grid (+ count 1) stop-count)))]
        [else 
         (set! prev-position robot) ;; Update the previous position
         (set! bad-moves (cons robot bad-moves))  ;; Add the current position to bad moves
         (dequeue)
         (set! robot next-robot)
         ;; Continue the search, clearing the queue at each step
         (search2 grid (+ count 1) stop-count)]))))

(define distance
  (lambda (node1 node2)
    (+ (abs (- (car node1) (car node2))) (abs (- (cadr node1) (cadr node2))))))

(define heuristic 
  (lambda (number point)
    (+ number (distance point goal))))

(define heuristic_compare 
  (lambda (node1 node2)
    (< (heuristic (length (get-path node1)) node1) 
       (heuristic (length (get-path node2)) node2))))

(define get-path
  (lambda (last-node)
    (cond
      [(equal? start last-node) (list start)]
      [else (cons last-node (get-path (cadr (assoc last-node path-lst))))])))

(define draw-path
  (lambda (path)
    (cond
      ((not (null? path))
         (draw-pt-path-node (car path))
         (draw-path (cdr path))))))

(define draw-pt-path-node
  (lambda (point)
    (draw-path-node (car point) (cadr point))))
