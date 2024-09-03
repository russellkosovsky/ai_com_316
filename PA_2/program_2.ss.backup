;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;         Scheme Programming Assignment #2        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;define sqr-list
;(sqr-list lst) => squares each element of the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    


(define sqr-list
  (lambda (lst)     ; anonymous function (lambda) that takes one list of numbers.
    (if (null? lst) ; the base case of the recursion
      '()           ; if lst is empty, return an empty list
        (cons (* (car lst) (car lst)) ; retrieves the first element of the list and squares it
                                      ; cons: makes new list by consing the squared first element to the list returned by the recursive call
          (sqr-list (cdr lst))))))    ; recursively calls the rest of the list to process the remaining elements

;sqr-list '(1 2 3)
;= (cons 1 (sqr-list '(2 3)))
;= (cons 1 (cons 4 (sqr-list '(3))))
;= (cons 1 (cons 4 (cons 9 (sqr-list '()))))
;= (cons 1 (cons 4 (cons 9 '())))
;= (1 4 9)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;define place: 
;(place x lst) => inserts x into ordered list lst
; place may be too hard; I'll get you started with one possible answer 
;(it uses a helper function):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    


(define place
  (lambda (x lst)
    (if (null? lst)  ; base case of the recursion --- if the list is empty, return a new list containing just x
      (list x)
      (place2 x (car lst) (cdr lst))))) ; the recursive case. If lst is not empty, it calls a helper function place2 with three arguments:
                                        ;   x: The element to be inserted.
                                        ;   carlst: The first element of the list lst.
                                        ;   cdrlst: The rest of the list lst

(define place2
  (lambda (x carlst cdrlst)
    (if (< x carlst) ; core comparison
      ; executes if x is less than carlst
      (cons x (cons carlst cdrlst)) ; inserts x before carlst and returns the new list
      ;executes if x is greater than or equal to carlst
      (cons carlst (place x cdrlst))))) ; continues the recursion, eventually leading to x being inserted in the appropriate position within cdrlst


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;(change x) => if x is negative it returns x^2,
;              if 0 it returns 0, 
;              if positive it returns x + 1.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    

(define change
  (lambda (x)
    (cond
      ((< x 0) (* x x)) ; if x is less than zero --- returns x squared
      ((= x 0) 0)       ; if x is equal to zero --- function returns 0
      (else (+ x 1))))) ; if x is positive, the function returns x + 1

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        
;(change-list lst) applies change to each element in the list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    

(define change-list
  (lambda (lst)
    
    ; base case of the recursion
    (if (null? lst) 
      '() ; if the list is empty, return an empty list
      
      ; recursive case -- processes the first element of the list using the change function, 
      ;                   then recursively processes the rest of the list
      (cons (change (car lst)) (change-list (cdr lst))))))
          ;car lst: gets first element of the list lst
          ;change (car lst): calls change function to the first element
          ;cdr lst: gets the rest of the list after removing the first element
          ;change-list (cdr lst): recursively calls change-list on the rest of the list for the remaining elements
          ;cons: makes it a new list by consing the modified first element to the list returned by the recursive call


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;(closest-point point lst) => returns the point in the list of points that 
;is closest to the input point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    
    
(define closest-point
  (lambda (point lst)
    
    (define distance ; helper function that calculates euclidean distance between two points
      (lambda (p1 p2)
        (sqrt (+ (expt (- (car p1) (car p2)) 2)
                 (expt (- (cadr p1) (cadr p2)) 2)))))
    
    (define closest  ; helper function that recursively traverses the list of points to find the closest point
      (lambda (point lst closest-point closest-dist)
        
        (if (null? lst)   ; Base Case:
            closest-point ;if lst empty (all points have been checked), return closest-point, the closest point found so far.
            
            (let* ((current-point (car lst)) ; evaluates expressions sequentially and binds the results to variables 
                                                                  ; The let* form is used because each step might depend on the previous one
                   (current-dist (distance point current-point))) ; computes the distance from point to current-point
              
              (if (< current-dist closest-dist) ; checks if the distance from point to current-point is less than the current shortest distance (closest-dist)
                  ; if True: update closest-point to current-point and closest-dist to current-dist.
                  (closest point (cdr lst) current-point current-dist)
                  (closest point (cdr lst) closest-point closest-dist))))))
                  ; if False: keep closest-point and closest-dist unchanged

    (closest point lst (car lst) (distance point (car lst))))) ; starts the process with the first point in the list as the initial closest-point 
                                                               ; the distance from point to this first point becomes the initial closest-dist

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;define add-list:
;(add-list lst1 lst2) => adds each element of the 2 lists
;    example:(add-list '(1 2 3) '(2 1 4))
;    returns:(3 3 7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


(define add-list
  (lambda (lst1 lst2)
    ; base case
    (if (or (null? lst1) (null? lst2))
        '() ; If either list is empty, return an empty list

        ; recursive case -- adds the first elements of both lists together and recursively processes the rest of the lists
        (cons (+ (car lst1) (car lst2))
              ; recursively calls add-list on the remaining elements of lst1 and lst2
              (add-list (cdr lst1) (cdr lst2))))))



    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;define delete-lists:
;(delete-lists lst) => deletes the sub-lists from this list.
;example:(delete-lists '(1 2 (3 4) (5 (6 7)) 8 (9)))
;returns: (1 2 8)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


(define delete-lists
  (lambda (lst)

    ; base case -- If lst is empty, return an empty list
    (if (null? lst)
      '()
        
        ; binds the first element of lst to the variable first
        (let ((first (car lst)))

          ; Returns #t if first is a list, otherwise #f
          (if (list? first)

            ; Recursively calls delete-lists on the rest of the list (cdr lst), which excludes the first element
            (delete-lists (cdr lst))
            ; Constructs a new list by consing the first element (if it’s not a list) onto the result of the recursive call to delete-lists on the rest of the list
            (cons first (delete-lists (cdr lst))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
;define flatten:
;(flatten lst) => removes sub-list structure but keeps the list elements
;example:(flatten '(1 2 (3 (4 5) 6))) 
;returns: (1 2 3 4 5 6)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


(define flatten
  (lambda (lst)
    ; base case
    (if (null? lst)
        '()
        ; Binds the first element of lst to the variable first
        (let ((first (car lst)))

          ; Returns #t if first is a list, otherwise #f
          (if (list? first)

              ; where first is a sub-list
              (append (flatten first) (flatten (cdr lst))) 
              ; Recursively flattens the sub-list 'first'
              ; recursively flattens the rest of the list 'cdr lst'
              ; combines the flattened version of first with the flattened rest of the list

              ; where first is not a sub-list
              (cons first (flatten (cdr lst)))))))) 
              ; makes a new list by consing the first element onto the flattened version of the rest of the list cdr lst


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;Run 20 commands from Chapter 6 that you think would be particularly
;advantageous.  Make sure you experiment with vectors and their conversions
;to and from lists.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


;procedure: (vector obj ...)
;returns: a vector of the objects obj ...
(vector)          ;=> #()
(vector 'a 'b 'c) ;=> #(a b c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (make-vector n)
;procedure: (make-vector n obj)
;returns: a vector of length n
;    n must be an exact nonnegative integer. 
;    If obj is supplied, each element of the vector
;    is filled with obj; otherwise, the elements are unspecified.

(make-vector 0)       ;=> #()
(make-vector 0 '#(a)) ;=> #()
(make-vector 5 '#(a)) ;=> #(#(a) #(a) #(a) #(a) #(a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (vector-length vector)
;returns: the number of elements in vector
;	   The length of a vector is always an exact nonnegative integer.

(vector-length '#())                      ; => 0
(vector-length '#(a b c))                 ; => 3 
(vector-length (vector 1 '(2) 3 '#(4 5))) ; => 4
(vector-length (make-vector 300))         ; => 300

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (vector-ref vector n)
;returns: the nth element (zero-based) of vector
;    n must be an exact nonnegative integer less than the length of vector.

(vector-ref '#(a b c) 0)  ; => a
(vector-ref '#(a b c) 1)  ; => b
(vector-ref '#(x y z w) 3); => w

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (vector-set! vector n obj)
;returns: unspecified
;    n must be an exact nonnegative integer less than the length of vector. vector-set!
;    changes the nth element of vector to obj.

(let ([v (vector 'a 'b 'c 'd 'e)])
     (vector-set! v 2 'x)
v) ; => #(a b x d e)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (vector-fill! vector obj)
;returns: unspecified
;    vector-fill! replaces each element of vector with obj. It may be defined without
;    error checks as follows.

(define vector-fill!
  (lambda (v x)
    (let ([n (vector-length v)])
      (do ([i 0 (+ i 1)])
        ((= i n))
      (vector-set! v i x)))))

(let ([v (vector 1 2 3)])
  (vector-fill! v 0)
  v) ; => #(0 0 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (vector->list vector)
;returns: a list of the elements of vector
;   vector->list provides a convenient method for applying list-processing operations
;   to vectors. It may be defined without error checks as follows.
(define vector->list
  (lambda (s)
    (do ([i (- (vector-length s) 1) (- i 1)]
      [ls '() (cons (vector-ref s i) ls)])
      ((< i 0) ls))))

(vector->list (vector))  ; => ()
(vector->list '#(a b c)) ; => (a b c)

(let ((v '#(1 2 3 4 5)))
  (apply * (vector->list v))) ; => 120

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;procedure: (list->vector list)
;returns: a vector of the elements of list
;   list->vector is the functional inverse of vector->list. The two procedures are
;   often used in combination to take advantage of a list-processing operation. A vector
;   may be converted to a list with vector->list, this list processed in some manner to
;   produce a new list, and the new list converted back into a vector with list->vector.

;   list->vector may be defined without error checks as follows.

(define list->vector
  (lambda (ls)
    (let ([s (make-vector (length ls))])
      (do ([ls ls (cdr ls)] [i 0 (+ i 1)])
        ((null? ls) s)
      (vector-set! s i (car ls))))))

(list->vector '())      ; => #()
(list->vector '(a b c)) ; => #(a b c)

(let ([v '#(1 2 3 4 5)])
  (let ([ls (vector->list v)])
    (list->vector (map * ls ls)))) ; => #(1 4 9 16 25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      



