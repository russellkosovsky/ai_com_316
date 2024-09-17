
(define queue '())  ;initialize queue

(define front
  (lambda ()
    (if (null? queue)
        '()  ;eeturn empty if queue is empty
    ;else
        (car queue))))  ;return front element

(define dequeue
  (lambda ()
    (if (null? queue)
        '()
    ; else   
        (let ((temp (front)))  ;get front element
          (set! queue (cdr queue))  ;remove front element from queue
          temp))))  ;return dequeued element

(define enqueue
  (lambda (lst)
    (if (not (null? lst))
        (let ((first (car lst)))  ;get first element of the list
          ;(display "Enqueuing point: ") (display first) (newline)  ;print the point
          (set! queue (add-to-queue first queue))  ;add it to the queue in order
          ;(display "Queue after enqueue: ") (display queue) (newline)
          (enqueue (cdr lst))))))  ;recursively enqueue the rest of the list

(define add-to-queue 
  (lambda (point qlst)
    (if (null? qlst)
        (list point)  ;if queue is empty, just return the point in a list
    ; else
        (let ((first (car qlst)))  ;get the first element of the queue
          (if (<= (get-key point) (get-key first))  ;compare based on heuristic
              (cons point qlst)  ; if its priority is lower -- insert the point before the first element
          ; else continue inserting
              (cons first (add-to-queue point (cdr qlst))))))))

(define get-key
  (lambda (point)
    (distance point goal)))  ;use distance as the heuristic
