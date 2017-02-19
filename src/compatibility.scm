;; Utilities used for incompatible environments.

(define (for-each proc seq)
  (if (null? seq)
      'done
      (begin
        (proc (car seq))
        (for-each (cdr seq) proc))))

(define (find-tail pred seq)
  (cond ((null? seq) #f)
        ((pred (car seq)) seq)
        (else (find-tail pred (cdr seq)))))

(define (find pred seq)
  (cond ((null? seq) #f)
        ((pred (car seq)) (car seq))
        (else (find pred (cdr seq)))))

(define (fold-left op init lst)
  (if (null? lst)
      init
      (fold-left op (op init (car lst)) (cdr lst))))
