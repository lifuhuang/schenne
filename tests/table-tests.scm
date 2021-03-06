(load "src/data-structures/table.scm")
(load "tests/test-utils.scm")

;; test tables

(define (test-table)
  (define t (make-table))
  (put! t "tom" 15)
  (assert-equal (get t "tom") 15)
  (put! t "jack" 17)
  (assert-equal (get t "jack") 17)
  (put! t "tom" 16)
  (assert-equal (get t "tom") 16)
  (assert-equal (get t "jack") 17)
  (write "."))

(define (run-table-tests)
  (newline)
  (test-table)
  (write "Passed table tests."))

