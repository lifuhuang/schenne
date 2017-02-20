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
  (list 'table))

(define (put! table key value)
  (let ((tail (find-tail 
                (lambda (x) (equal? key (item-key x)))
                (cdr table))))
    (if tail
      (set-item-value! (car tail) value)
      (set-cdr! table (cons 
                        (make-item key value) 
                        (cdr table))))))

(define (has-key? table key)
  (let ((tail (find-tail 
                (lambda (x) (equal? key (item-key x)))
                (cdr table))))
    (if tail #t #f)))

(define (get table key)
  (let ((tail (find-tail 
                (lambda (x) (equal? key (item-key x)))
                (cdr table))))
    (if tail
      (item-value (car tail)) 
      (error "Cannot find item with key" key))))

