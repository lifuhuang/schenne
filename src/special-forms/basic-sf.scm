(load "src/tags.scm")

(define (install-quotation syntax)
  (define (analyze-quotation expr)
    (lambda (env)
      (cdr expr)))

  (register 'quote analyze-quotation syntax))

(define (install-assignment syntax)
  (define (analyze-assignment expr)
    (let ((var (cadr expr))
          (val-exe (analyze (caddr expr))))
      (lambda (env)
        (set-variable! var (val-exe env) env))))
  
  (register 'assignment analyze-assignment syntax))

(define (install-definition syntax)
  (define (analyze-definition expr)
    (let ((var (cadr expr))
          (val-exe (analyze (caddr expr))))
      (lambda (env)
        (define-variable! var (val-exe env) env))))
  
  ;; interface
  (register 'definition analyze-definition syntax))

(define (install-if-expression syntax)
  (define (analyze-if-expression expr)
    (let ((predicate-exe (analyze expr syntax))
          (consequence-exe (analyze expr syntax))
          (alternative-exe (analyze expr syntax)))
      (lambda (env)
        (if (true? (predicate-exe env))
          (consequence-exe env)
          (alternative-exe env)))))
          
  (register 'if analyze-if-expression syntax))

(define (install-lambda-expression syntax)
  (define (analyze-lambda-expression expr syntax)
    (lambda (env)
      (make-compound-procedure (cadr expr) (caddr expr) env)))
          
  (register 'lambda analyze-lambda-expression syntax))

(define (chain-executables exes)
  (let ((sequentially (lambda (exe1 exe2)
                        (lambda (env)
                          (exe1 env)
                          (exe2 env))))
        (dummy-exe (lambda (env)
                     unspecified-value)))
    (reduce-left sequentially dummy-exe exes)))

(define (install-begin-expression syntax)
  (define (analyze-begin-expression expr syntax)
    (chain-executables (map (lambda (expr) 
                              (analyze expr syntax)) 
                            (cdr expr))))
          
  (register 'begin analyze-begin-expression syntax))

(define (install-cond-expression syntax)
  (define (analyze-cond-expression expr)
    (define (analyze-clause-predicate clause)
      (let ((predicate (car clause)))
        (if (eq? predicate 'else)
          (lambda (env) #t)
          (analyze predicate syntax))))

    (define (analyze-clause-consequence clause)
      (let* ((consequence-exprs (cdr clause))
             (executables (map (lambda (expr) 
                                 (analyze expr syntax)) 
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
                (else unspecified-value-tag)))))

    (let ((clauses (cdr expr)))
      (cond ((null? clauses) 
             (error "cond must have at least one clause."))
            ((invalid-else? clauses)
             (error "Invalid else clause."))
            (else
              (reduce-right merge-clauses
                            '()
                            clauses)))))

  (register 'cond analyze-cond-expression syntax))

