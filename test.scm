(load "utils.scm")
(load "table.scm")

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

(define (assert-equal exp-a exp-b)
  (assert (eq? exp-a exp-b) exp-a "does not equals" exp-b))

(define (assert-error exp)
  (assert (eq? exp 'Error) exp "is returned instead of a expected error"))

(define (test-assertion . exp)
  (left-aggregate
   exp
   (lambda (x y) (and x (not (eq? y assertion-failed))))
   #t))


;; Test tables

(define (test-table)
  (define t (make-table))
  (assert-error (get t "Tom"))
  (put! t "Tom" 15)
  (assert-equal (get t "Tom") 15)
  (put! t "Jack" 17)
  (assert-equal (get t "Jack") 17)
  (put! t "Tom" 16)
  (assert-equal (get t "Tom") 16)
  (assert-equal (get t "Jack") 17)
  'Done)

(test-table)