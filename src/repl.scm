(load "src/core.scm")
(load "src/data-structures/procedure.scm")
(load "src/data-structures/environment.scm")

(define (lt x y)
  (if (< x y)
    'true
    'false))

(define (gt x y)
  (if (> x y)
    'true
    'false))

(define (le x y)
  (if (<= x y)
    'true
    'false))

(define (ge x y)
  (if (>= x y)
    'true
    'false))

(define (setup-environment)
  (extend-environment 
    the-empty-environment
    (make-frame 
      (list
        (cons '+ (make-primitive-proc +))
        (cons '- (make-primitive-proc -))
        (cons '* (make-primitive-proc *))
        (cons '/ (make-primitive-proc /))
        (cons '= (make-primitive-proc =))
        (cons '< (make-primitive-proc lt))
        (cons '> (make-primitive-proc gt))
        (cons '<= (make-primitive-proc le))
        (cons '>= (make-primitive-proc ge))
        (cons 'square (make-primitive-proc (lambda (x) (* x x))))
        (cons 'cube (make-primitive-proc (lambda (x) (* x x x))))
        (cons 'inc (make-primitive-proc (lambda (x) (+ x 1))))
        (cons 'dec (make-primitive-proc (lambda (x) (- x 1))))
        (cons 'cons (make-primitive-proc cons))
        (cons 'car (make-primitive-proc car))
        (cons 'cdr (make-primitive-proc cdr))
        (cons 'list (make-primitive-proc list))))))

(define (run-repl)
  (let ((env (setup-environment)))
    (define (loop)
      (newline)
      (write ">>")
      (write (schenne-eval (read) env))
      (loop))
    (loop)))

(run-repl)

