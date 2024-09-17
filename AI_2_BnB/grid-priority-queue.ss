
(define queue '())  ;initialize queue

(define front
  (lambda ()
    (if (null? queue)
      '()  ;eeturn empty if queue is empty
        (car queue))))  ;else return front element

(define dequeue
  (lambda ()
    (if (null? queue)
        '()   
        (let ((temp (front)))  ;else get front element
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
          (if (<= (get-key point) (get-key first))  ;compare if its heuristic priority is lower 
              (cons point qlst)  ;insert the point before the first element
              (cons first (add-to-queue point (cdr qlst)))))))) ;else continue inserting

(define get-key
  (lambda (point)
    (distance point goal)))  ;use distance as the heuristic
