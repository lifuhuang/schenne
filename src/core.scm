(load "src/builtins.scm")
(load "src/special-forms/basic-sf.scm")
(load "src/data-structures/syntax-table.scm")
(load "src/data-structures/procedure.scm")
(load "src/data-structures/environment.scm")

(define syntax-table (make-syntax-table))

(install-all-special-forms syntax-table)

(define (analyze expr syntax-table)
  (define (analyze-primitive-expr)
    (if (or (number? expr) (string? expr) (bool? expr))
      (lambda (env) expr)
      (lambda (env) (find-variable expr env))))

  (define (analyze-complex-expr)
    (let ((operator (car expr))
          (operands (cdr expr)))
      (define (analyze-proc-application)
        (let ((operator-exe (analyze operator syntax-table))
              (operands-exes (map (lambda (expr)
                                    (analyze expr syntax-table))
                                  operands)))
          (lambda (env)
            (schenne-apply (operator-exe env)
                           (map (lambda (exe) (exe env))
                                operands-exes)))))

      (let ((analyze-special-form (and (symbol? operator)
                                       (lookup operator syntax-table))))
        (if analyze-special-form
          (analyze-special-form expr syntax-table)
          (analyze-proc-application)))))

  (if (pair? expr)
    (analyze-complex-expr)
    (analyze-primitive-expr)))

(define (schenne-eval expr env)
    ((analyze expr syntax-table) env))

(define (schenne-apply proc args)
  (define (apply-primitive-proc)
    (apply (cadr proc) args))

  (define (apply-compound-proc)
    ((compound-proc-body proc) (extend-environment
                                 (compound-proc-env proc)
                                 (make-frame (map cons
                                                  (compound-proc-parameters proc)
                                                  args)))))

  (if (primitive-proc? proc)
    (apply-primitive-proc)
    (apply-compound-proc)))

(define (setup-environment)
  (extend-environment
    the-empty-environment
    (make-frame (map (lambda (p)
                       (cons (car p)
                             (make-primitive-proc (cdr p))))
                     builtin-list))))
