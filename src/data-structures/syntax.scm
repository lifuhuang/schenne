(load "src/data-structures/table.scm")

(define (make-syntax) (make-table))

(define (register op proc syntax)
  (put! syntax op proc))

(define (lookup op syntax)
  (if (has-key? syntax op)
    (get syntax op)
    #f))

