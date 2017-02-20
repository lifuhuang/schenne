;; Data structures for modeling environments.

;; Frame
(define (make-frame bindings)
  (cons 'Head bindings))

(define (frame-bindings frame)
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
                    (frame-bindings (first-frame env)))))
        (if tail
          (cdar tail)
          (iter (cdr env))))))
  (iter env)) 

(define (set-variable! var value env)
  (define (iter env)
    (if (eq? env the-empty-environment)
      (error "Undefined variable: " var)
      (let ((tail (find-tail
                    (lambda (binding) (eq? (car binding) var))
                    (frame-bindings (first-frame env)))))
        (if tail
          (set-cdr! (car tail) value)
          (iter (cdr env))))))
  (iter env))

(define (define-variable! var value env)
  (let ((frame (first-frame env)))
    (let ((tail (find-tail
                  (lambda (binding) (eq? (car binding) var))
                  (frame-bindings frame))))
      (if tail
        (set-cdr! (car tail) value)
        (add-binding-to-frame! frame var value)))))

(define (extend-environment env frame)
  (cons frame env))

