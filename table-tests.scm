(load "test.scm")
(load "table.scm")

;; test tables

(define (test-table)
  (define t (make-table))
  (put! t "tom" 15)
  (assert-equal (get t "tom") 15)
  (put! t "jack" 17)
  (assert-equal (get t "jack") 17)
  (put! t "tom" 16)
  (assert-equal (get t "tom") 16)
  (assert-equal (get t "jack") 17))
