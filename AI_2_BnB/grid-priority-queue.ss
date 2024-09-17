(define queue '())  ; initialize queue

(define front
  (lambda ()
    (if (null? queue)
        '()  ; eeturn empty if the queue is empty
    ; else
        (car queue))))  ; returns the front element

(define dequeue
  (lambda ()
    (if (null? queue)
        '()  ; return empty if the queue is empty
    ; else   
        (let ((temp (front)))  ; get the front element
          (set! queue (cdr queue))  ; remove the front element from the queue
          temp))))  ; return the dequeued element

(define enqueue
  (lambda (lst)
    (if (not (null? lst))
        (let ((first (car lst)))  ;get the first element of the list
          ;(display "Enqueuing point: ") (display first) (newline)  ;print the point
          (set! queue (add-to-queue first queue))  ;add it to the queue in order
          ;(display "Queue after enqueue: ") (display queue) (newline)  ;print the queue
          (enqueue (cdr lst))))))  ;recursively enqueue the rest of the list

(define add-to-queue 
  (lambda (point qlst)
    (if (null? qlst)
        (list point)  ; rf the queue is empty, just return the point in a list
    ; else
        (let ((first (car qlst)))  ; get the first element of the queue
          (if (<= (get-key point) (get-key first))  ; compare based on heuristic
              (cons point qlst)  ; insert the point before the first element if its priority is lower
          ; else
              (cons first (add-to-queue point (cdr qlst))))))))  ; otherwise, continue inserting

(define get-key
  (lambda (point)
    (distance point goal)))  ; use the distance as the heuristic
