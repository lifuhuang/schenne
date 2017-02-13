(load "utils.scm")

;; Key-value table

(define (make-item key value)
  (cons key value))

(define (get-item-key item)
  (car item))

(define (get-item-value item)
  (cdr item))

(define (set-item-value! item value)
  (set-cdr! item value))

(define (make-table)
  (cons 0 '()))

(define (table-size table)
  (car table))

(define (table-content table)
  (cdr table))

(define (find-in-table table key)
  (define (iter lst)
    (cond ((null? lst) lst)
          ((eq? (get-item-key (car lst)) key) (car lst))
          (else (iter (cdr lst)))))
  (iter (table-content table)))
      
(define (put! table key value)
  (let ((result (find-in-table table key)))
    (if (null? result)
        (begin
          (set-car! table (+ (table-size table) 1))
          (set-cdr! table (cons
                           (make-item key value)
                           (table-content table))))
        (set-item-value! result value))))

(define (get table key)
  (let ((result (find-in-table table key)))
    (if (null? result)
        (error "Cannot find item with key" key)
        (get-item-value result))))
