(load "src/globals.scm")

;; All basic special forms in Scheme.

(define unspecified-value 'unspecified-value)

(define (install-quotation syntax-table)
  (define (analyze-quotation expr syntax-table)
    (lambda (env)
      (cdr expr)))

  (register 'quote analyze-quotation syntax-table))

(define (install-assignment syntax-table)
  (define (analyze-assignment expr syntax-table)
    (let ((var (cadr expr))
          (val-exe (analyze (caddr expr) syntax-table)))
      (lambda (env)
        (set-variable! var (val-exe env) env))))

  (register 'set! analyze-assignment syntax-table))

(define (install-definition syntax-table)
  (define (analyze-definition expr syntax-table)
    (define (analyze-var-definition)
      (let ((var (cadr expr))
            (val-exe (analyze (caddr expr) syntax-table)))
        (lambda (env)
          (define-variable! var (val-exe env) env))))

    (define (analyze-proc-definition)
      (let ((proc-name (caadr expr))
            (proc-params (cdadr expr))
            (proc-body (chain-executables (map (lambda (expr)
                                                  (analyze expr syntax-table))
                                                  (cddr expr)))))
        (lambda (env)
          (define-variable! proc-name
                            (make-compound-proc proc-params proc-body env)
                            env))))

    (if (pair? (cadr expr))
      (analyze-var-definition)
      (analyze-proc-definition)))

  (register 'define analyze-definition syntax-table))

(define (install-if-expression syntax-table)
  (define (analyze-if-expression expr syntax-table)
    (let ((predicate-exe (analyze (cadr expr) syntax-table))
          (consequence-exe (analyze (caddr expr) syntax-table))
          (alternative-exe (analyze (cadddr expr) syntax-table)))
      (lambda (env)
        (if (true? (predicate-exe env))
          (consequence-exe env)
          (alternative-exe env)))))

  (register 'if analyze-if-expression syntax-table))

(define (install-lambda-expression syntax-table)
  (define (analyze-lambda-expression expr syntax-table)
    (let ((executable (chain-executables
                        (map (lambda (expr)
                               (analyze expr syntax-table))
                             (cddr expr)))))
    (lambda (env)
      (make-compound-proc (cadr expr) executable env))))

  (register 'lambda analyze-lambda-expression syntax-table))

(define (install-begin-expression syntax-table)
  (define (analyze-begin-expression expr syntax-table)
    (chain-executables (map (lambda (expr)
                              (analyze expr syntax-table))
                            (cdr expr))))

  (register 'begin analyze-begin-expression syntax-table))

(define (install-cond-expression syntax-table)
  (define (analyze-cond-expression expr syntax-table)
    (define (analyze-clause-predicate clause)
      (let ((predicate (car clause)))
        (if (eq? predicate 'else)
          (lambda (env) #t)
          (analyze predicate syntax-table))))

    (define (analyze-clause-consequence clause)
      (let* ((consequence-exprs (cdr clause))
             (executables (map (lambda (expr)
                                 (analyze expr syntax-table))
                               consequence-exprs)))
        (chain-executables executables)))

    (define (invalid-else? clauses)
      (cond ((null? clauses) #f)
            ((and (eq? (caar clauses) 'else)
                  (not (eq? (cdr clauses) '())))
             #t)
            (else (invalid-else? (cdr clauses)))))

    (define (merge-clauses clause1 clause2)
      (let ((pred1 (analyze-clause-predicate clause1))
            (pred2 (analyze-clause-predicate clause2))
            (conseq1 (analyze-clause-consequence clause1))
            (conseq2 (analyze-clause-consequence clause2)))
        (lambda (env)
          (cond ((true? pred1) (conseq1 env))
                ((true? pred2) (conseq2 env))
                (else unspecified-value)))))

    (let ((clauses (cdr expr)))
      (cond ((null? clauses)
             (error "cond must have at least one clause."))
            ((invalid-else? clauses)
             (error "Invalid else clause."))
            (else
              (reduce-right merge-clauses
                            '()
                            clauses)))))

  (register 'cond analyze-cond-expression syntax-table))

(define (install-all-special-forms syntax-table)
  (install-assignment syntax-table)
  (install-begin-expression syntax-table)
  (install-cond-expression syntax-table)
  (install-definition syntax-table)
  (install-if-expression syntax-table)
  (install-lambda-expression syntax-table)
  (install-quotation syntax-table))
