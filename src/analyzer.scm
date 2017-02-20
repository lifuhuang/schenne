(load "src/table.scm")
(load "src/special-forms/basic-sf.scm")

(define (make-syntax) (make-table))

(define (register op proc syntax)
  (put! syntax op proc))

(define (lookup op syntax)
  (if (has-key? syntax op)
    (get syntax op)
    #f))

(define (bool? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)))

(define (analyze expr syntax)
  (define (analyze-primitive-expr)
    (if (or (number? expr) (string? expr) (bool? expr))
      (lambda (env) expr)
      (lambda (env) (find-variable expr env))))

  (define (analyze-complex-expr)
    (let ((operator (car expr)) (operands (cdr expr)))
      (define (analyze-proc-application)
        (lambda (env)
          (let (proc (find-variable operator env))
            (if (primitive-procedure? proc)
              (apply-primitive-procedure proc operands)
              (apply-compound-procedure proc operands)))))

      (let ((analyze-special-form (lookup operator syntax)))
        (if analyze-special-form
          (analyze-special-form expr syntax)
          (analyze-proc-application)))))

  (if (pair? expr)
    (analyze-complex-expr)
    (analyze-primitive-expr)))

