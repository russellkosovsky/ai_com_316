(define randomize
  (lambda (lst)
    (let ((size (length lst)))
      (cond 
        ((< size 2) lst)
        (else
          (let ((node-num (random size)))
            (cons (list-ref lst node-num) 
                  (randomize (removex node-num lst)))))))))

(define removex
  (lambda (num lst)
    (if (= num 0)
      (cdr lst)
    ;else
      (cons (car lst) (removex (- num 1) (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-next-goal0 
  (lambda (point)
      (if (< (+ (abs (- (car robot) (car point))) 
                (abs (- (cadr robot) (cadr point)))) 2)
          point
      ;else
         (get-next-goal point))))

(define get-next-goal 
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point)))
           (lst0 (randomize lst1))
           (flst (calculate-h-goal lst0))
           (lst (map list flst lst0))) 
      (set! queue '())
      (enqueue lst)
      (set! queue (reverse queue))
      (let ((num (random 10))
            (len (length lst0))
            (best (front)))
         (cond 
           ((= num 0)
               (list-ref lst0 (random len))) 
            (else
               best))))))
 
(define calculate-h-goal
  (lambda (lst)
    (map h-goal lst)))

(define h-goal
  (lambda (point)
    (+ (abs (- (car point) (car robot)))
       (abs (- (cadr point) (cadr robot))))))   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-next-robot 
  (lambda (point)
    (let* ((lst1 (cons point (adjacento point))) 
           (lst0 (randomize lst1))
           (flst (calculate-h lst0))
           (lst (map list flst lst0))) 
      (set! queue '())
      (enqueue lst)
      (let ((num (random 10))
            (len (length lst0))
            (best (front)))
         (cond 
           ((= num 0)
              (list-ref lst0 (random len))) 
           (else
              best))))))

            
(define calculate-h
  (lambda (lst)
    (map h lst)))

(define h
  (lambda (point)
    (+ (abs (- (car point) (car goal)))
       (abs (- (cadr point) (cadr goal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            
(define search
  (lambda (grid stop-count)
    (search2 grid 1 stop-count)))

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
      (else
        (pause pause-num)
        (search-robot grid)
        (if (null? robot)
          (display "Cannot reach the goal")
          (begin
            (pause pause-num)
            (search-goal grid)
            (search2 grid (+ count 1) stop-count)))))))

(define search-robot
  (lambda (grid)
    (let ((next-robot (get-next-robot robot)))
      (set! robot next-robot)
      (if (not (null? robot))
        (draw-moved-robotx robot)))))

(define search-goal
  (lambda (grid)
    (let ((next-goal (get-next-goal0 goal)))
      (set! goal next-goal)
      (draw-moved-goalx goal))))