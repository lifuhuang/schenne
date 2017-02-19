(load "src/table.scm")


(define (make-syntax) (make-table))

(define (register op type proc syntax)
  (put! syntax (cons op type) proc))

(define (lookup op type syntax)
  (get syntax (cons op type)))

(define (install-self-evaluating syntax)
  ;; internal procedure
  (define (bool? expr)
    (or (eq? expr 'true)
        (eq? expr 'false)))

  (define (test expr)
    (or (number? expr)
        (string? expr)
        (bool? expr)))

  (define (analyze expr) 
    (lambda (env) expr))

  ;; interface
  (register 'self-evaluating 'test test syntax)
  (register 'self-evaluating 'analyze analyze syntax))

(define (install-variable syntax)
  ;; internal procedure
  (define (test expr)
    (symbol? expr))

  (define (analyze expr)
    (lambda (env)
      (find-variable expr env)))
  
  ;; interface
  (register 'self-evaluating 'test test syntax)
  (register 'self-evaluating 'analyze analyze syntax))

(define (install-quotation syntax)
  ;; internal procedure
  (define (test expr)
    (tagged-with? expr 'quote))

  (define (analyze expr)
    (lambda (env)
      (cdr expr)))
  
  ;; interface
  (register 'self-evaluating 'test test syntax)
  (register 'self-evaluating 'analyze analyze syntax))

(define (install-assignment syntax)
  ;; internal procedure
  (define (test expr)
    (tagged-with? expr 'set!))

  (define (analyze expr)
    (lambda (env)
      (set
  
  ;; interface
  (register 'self-evaluating 'test test syntax)
  (register 'self-evaluating 'analyze analyze syntax))



(define (analyze expr syntax)
  (cond ((primitive-value? expr) (analyze-primitive-value expr))
        ((variable? expr) (analyze-variable expr))
        ((quotation? expr) (analyze-quotation expr))
        ((assignment? expr) (analyze-assignment expr))
        ((definition? expr) (analyze-definition expr))
        ((if-expression? expr) (analyze-if-expression expr))
        ((lambda-expression? expr) (analyze-lambda-expression expr))
        ((begin-expression? expr) (analyze-begin-expression expr))
        ((cond-expression? expr) (analyze-cond-expression expr))
        ((application? expr) (analyze-application expr))
        (else (error "Unknown expression:" expr))))

(define (analyze-primitive-value expr)
  (

