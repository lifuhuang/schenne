;; Key-value table

(define (make-item key value)
  (cons key value))

(define (item-key item)
  (car item))

(define (item-value item) 
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
                (lambda (x) (string=? key (item-key x)))
                (table-content table))))
    (if tail
      (set-item-value! (car tail) value)
      (begin
          (set-car! table (+ (table-size table) 1))
          (set-cdr! table (cons 
                            (make-item key value) 
                            (table-content table))))))) 

(define (get table key)
  (let ((tail (find-tail 
                (lambda (x) (string=? key (item-key x)))
                (table-content table))))
    (if tail
      (item-value (car tail)) 
      (error "Cannot find item with key" key))))

