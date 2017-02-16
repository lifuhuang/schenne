(load "utils.scm")

;; Test utils
(define assertion-failed 'ASSERTION-FAILED)
(define assertion-passed 'ASSERTION-PASSED)
(define (assert exp . msg)
  (if exp
      assertion-passed
      (begin
        (apply error
               (cons "assertion failed!" msg))
        assertion-failed)))

(define (assert-eq exp-a exp-b)
  (assert (eq? exp-a exp-b) exp-a "does not eq" exp-b))

(define (assert-eqv exp-a exp-b)
  (assert (eqv? exp-a exp-b) exp-a "does not eqv" exp-b))

(define (assert-equal exp-a exp-b)
  (assert (equal? exp-a exp-b) exp-a "does not equal" exp-b))

(define (test-assertion . exp)
  (left-aggregate
   exp
   (lambda (x y) (and x (not (eq? y assertion-failed))))
   #t))

