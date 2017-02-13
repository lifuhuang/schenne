
;; Utililies used by Schenne

(define (for-each lst proc)
  (if (null? lst)
      'Done
      (begin
        (proc (car lst))
        (for-each (cdr lst) proc))))

(define (left-aggregate lst op init)
  (if (null? lst)
      init
      (left-aggregate (cdr lst) op (op init (car lst)))))
        
;; Error logging
(define (error . strs)
  (write "ERROR:")
  (for-each
   strs
   (lambda (msg)
     (write " ")
     (write msg)))
  (newline)
  'Error)

