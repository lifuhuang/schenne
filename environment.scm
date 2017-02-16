;; Data structures for modeling environments.

;; Frame
(define (make-frame)
  (cons 'Head '()))

(define (get-frame-bindings frame)
  (cdr frame))

(define (add-binding-to-frame! frame var value)
  (set-cdr! frame (cons (cons var value) (cdr frame))))

;; Environment

(define the-empty-environment '())

(define (first-frame env)
  (car env))

(define (rest-frames env)
  (cdr env))

(define (find-variable var env)
  (define (iter env)
    (if (eq? env the-empty-environment)
      (error "Undefined variable: " var)
      (let ((tail (find-tail 
                    (lambda (binding) (eq? (car binding) var)) 
                    (get-frame-bindings (first-frame env)))))
        (if (null? tail)
          (iter (cdr env)) 
          (cdar tail)))))
  (iter env)) 

(define (set-variable! var value env)
  (define (iter env)
    (if (eq? env the-empty-environment)
      (error "Undefined variable: " var)
      (let ((tail (find-tail
                    (lambda (binding) (eq? (car binding) var))
                    (get-frame-bindings (first-frame env)))))
        (if (null? tail)
          (iter (cdr env))
          (set-cdr! (car tail) value)))))
  (iter env))

(define (define-variable! var value env)
  (let ((frame (first-frame env)))
    (let ((tail (find-tail
                  (lambda (binding) (eq? (car binding) var))
                  (get-frame-bindings frame))))
      (if (null? tail)
        (add-binding-to-frame! frame var value)
        (set-cdr! (car tail) value)))))

(define (extend-environment env frame)
  (cons frame env))

