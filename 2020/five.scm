(use-modules (ice-9 textual-ports))

(define (diff min max)
  (ceiling (/ (- max min) 2)))

(define (desc lst min max hi lo)
  (cond
     ((eq? min max) min)
     ((null? lst) min)
     ((eq? (car lst) hi) (desc (cdr lst) min (- max (diff min max)) hi lo))
     ((eq? (car lst) lo) (desc (cdr lst) (+ min (diff min max)) max hi lo))
     (else (desc (cdr lst) min max hi lo))))

(define (row str)
  (desc (string->list str) 0 127 #\F #\B))

(define (col str)
  (desc (string->list str) 0 7 #\L #\R))

(define (seat-id str)
  (+ (col str) (* 8 (row str))))

(define (find-gap lst)
  (cond
   ((>= 2 (length lst)) 0)
   ((< 1 (- (cadr lst) (car lst))) (+ (car lst)(diff (car lst) (cadr lst))))
   (else (find-gap (cdr lst)))))

(define (star-one lst)
  (apply max (map seat-id lst)))

(define (star-two lst)
  (find-gap (sort (map seat-id lst) <)))

(define (accum port lst)
  (let ((next (get-line port)))
    (cond
     ((eof-object? next) lst)
     (else (accum port (cons next lst))))))

(define (read-input)
  (call-with-input-file "./input/5" (lambda (f) (accum f '()))))

(define (main)
  (let ((input (read-input)))
    (display "Answer 1: ")
    (display (star-one input))
    (newline)
    (display "Answer 2: ")
    (display (star-two input))
    (newline)))

;; examples

(define (test-example str lst)
  (equal? lst (list (row str) (col str) (seat-id str))))

(define examples
  (list "FBFBBFFRLR" "BFFFBBFRRR" "FFFBBBFRRR" "BBFFBBFRLL"))

(define (examples?)
  (and
   (test-example "FBFBBFFRLR" '(44 5 357))
   (test-example "BFFFBBFRRR" '(70 7 567))
   (test-example "FFFBBBFRRR" '(14 7 119))
   (test-example "BBFFBBFRLL" '(102 4 820))
   (eq? 820 (star-one examples))))
