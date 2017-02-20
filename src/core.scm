(load "src/special-forms/basic-sf.scm")
(load "src/data-structures/syntax.scm")
(load "src/data-structures/procedure.scm")

(define syntax (make-syntax))

(install-all-special-forms syntax)

(define (analyze expr syntax)
  (define (analyze-primitive-expr)
    (if (or (number? expr) (string? expr) (bool? expr))
      (lambda (env) expr)
      (lambda (env) (find-variable expr env))))

  (define (analyze-complex-expr)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (define (analyze-proc-application)
        (let ((operator-exe (analyze operator syntax))
              (operands-exes (map (lambda (expr) 
                                    (analyze expr syntax))
                                  operands)))
          (lambda (env)
            (schenne-apply (operator-exe env) 
                           (map (lambda (exe) (exe env))
                                operands-exes)))))

      (let ((analyze-special-form (and (symbol? operator) 
                                       (lookup operator syntax))))
        (if analyze-special-form
          (analyze-special-form expr)
          (analyze-proc-application)))))

  (if (pair? expr)
    (analyze-complex-expr)
    (analyze-primitive-expr)))

(define (schenne-eval expr env)
  (let ((executable (analyze expr syntax)))
    (executable env)))

(define (schenne-apply proc args)
  (define (apply-primitive-proc)
    (apply (cadr proc) args))

  (define (apply-compound-proc)
    (let ((frame (make-frame 
                   (zip (compound-proc-parameters proc) args))))
      (schenne-eval (compound-proc-body proc)
                    (extend-environment (compound-proc-env proc) frame))))

  (if (primitive-proc? proc)
    (apply-primitive-proc)
    (apply-compound-proc)))

