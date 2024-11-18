(define and-threshold-weights '(((1.5 1 1))))

(define or-threshold-weights '(((0.5 1 1))))
(define xor-threshold-weights '(((0.6 1 -.5) (0.6 -.5 1)) 
                               ((.4 1 1))))

(define NN
  (lambda (lst)
    ;(NN2 lst and-threshold-weights)))
    ;(NN2 lst or-threshold-weights)))
    (NN2 lst xor-threshold-weights)))
    ;(NN2 lst not-xor-threshold-weights)))

(define NN2
  (lambda (lst tw)
    ;(display lst)
    ;(newline)
    (if (null? tw)
        lst
        (let ((next-level (get-next-level lst (car tw))))
          (NN2 next-level (cdr tw))))))

(define get-next-level
  (lambda (lst twl)
    (if (null? twl)
        '()
        (cons (get-node lst (car twl)) (get-next-level lst (cdr twl))))))

(define get-node
  (lambda (lst twn)
    (let ((threshold (car twn))
          (weights (cdr twn)))
      (g (+ (get-activations lst weights) (- threshold))))))

(define get-activations
  (lambda (lst w)
    (if (null? lst)
        0
        (+ (* (car lst) (car w)) (get-activations (cdr lst) (cdr w))))))

; Replace the step function with sigmoid
;(define g
 ; (lambda (x)
  ;  (/ 1 (+ 1 (exp (- x)))))) ; Sigmoid function

; testing
(newline)
(newline)
(display '(0 0))
(display (NN '(0 0)))
(newline)
(display '(0 1))
(display (NN '(0 1)))
(newline)
(display '(1 0))
(display (NN '(1 0)))
(newline)
(display '(1 1))
(display (NN '(1 1)))
(newline)
