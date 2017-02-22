(load "src/data-structures/table.scm")

(define (make-syntax-table) (make-table))

(define (register op proc syntax-table)
  (put! syntax-table op proc))

(define (lookup op syntax)
  (if (has-key? syntax-table op)
    (get syntax-table op)
    #f))
