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

(define (put! table key value)
  (let ((tail (find-tail 
                (lambda (x) (string=? key (get-item-key x)))
                (table-content table))))
    (if (null? tail)
        (begin
          (set-car! table (+ (table-size table) 1))
          (set-cdr! table (cons
                           (make-item key value)
                           (table-content table))))
        (set-item-value! (car tail) value))))

(define (get table key)
  (let ((tail (find-tail 
                (lambda (x) (string=? key (get-item-key x)))
                (table-content table))))
    (if (null? tail)
        (error "Cannot find item with key" key)
        (get-item-value (car tail)))))

