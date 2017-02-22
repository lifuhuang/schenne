(load "src/core.scm")
(load "tests/test-utils.scm")

(define (test-if-expression)
  (let ((env (setup-environment)))
    (assert-eq (schenne-eval '(if true 1 0) env) 1)
    (write ".")
    (assert (schenne-eval '(if false 1 0) env) 0)
    (write ".")
    (assert-eq (schenne-eval '(if (< 1 2) 1 0) env) 1)
    (write ".")
    (assert-eq (schenne-eval '(if (> 1 2) 1 0) env) 0)))
    (write ".")

(define (test-definition)
  (let ((env (setup-environment)))
    (schenne-eval '(define x 5) env) 
    (write ".")
    (schenne-eval '(define y 10) env) 
    (write ".")
    (assert-eq (schenne-eval 'x env) 5)
    (write ".")
    (assert-eq (schenne-eval 'y env) 10)
    (write ".")
    (schenne-eval '(define x 1) env) 
    (write ".")
    (assert-eq (schenne-eval 'x env) 1)
    (write ".")
    (assert-eq (schenne-eval 'y env) 10)
    (write ".")
    (schenne-eval '(define y 2) env) 
    (write ".")
    (assert-eq (schenne-eval 'x env) 1)
    (write ".")
    (assert-eq (schenne-eval 'y env) 2)
    (write ".")
    (schenne-eval '(define (func x y) (+ x y)) env)
    (write ".")
    (assert-eq (schenne-eval '(func 1 2) env) 3)
    (write ".")
    (schenne-eval '(define (method) 1) env)
    (write ".")
    (assert-eq (schenne-eval '(method) env) 1)
    (write ".")))

(define (test-lambda-environment)
  (let ((env (setup-environment)))
    (assert-eq (schenne-eval '((lambda () 1)) env) 1)
    (write ".")
    (assert-eq (schenne-eval '((lambda (x) (* x x x x)) 2) env) 16)
    (write ".")
    (assert-eq (schenne-eval '((lambda (x y) (* x y)) 4 8) env) 32)
    (write ".")
    (assert-eq (schenne-eval '((lambda (x y z) (if (> x 0) y z)) 1 0 1) env) 0)
    (write ".")
    (assert-eq (schenne-eval '((lambda (x y z) (if (> x 0) y z)) -1 0 1) env) 1)
    (write ".")
    ;; Multiple expression.
    (assert-eq (schenne-eval '((lambda (x y z) x (+ x y) (+ x y z)) 1 2 3) env) 6)
    (write ".")
    ;; Inner definition.
    (define y 100)
    (assert-eq (schenne-eval '((lambda (x) (define y 6) (+ x y)) 10) env) 16)
    (assert-eq y 100)
    (write ".")
    ;; Test closure.
    (assert (schenne-eval '(((lambda (x) (define y (+ x 2)) (lambda (z) (+ y z))) 1) 3) env) 6)
    (write ".")))

(define (test-cond-expression)
  (let ((env (setup-environment)))
    (assert-eq (schenne-eval 
                 '(cond (true 1))
                 env) 
               1)
    (write ".")
    (assert-eq (schenne-eval 
                 '(cond (false 1) 
                        (true 2))
                 env) 
               2)
    (write ".")
    (assert-eq (schenne-eval 
                 '(cond ((= 1 3) 1) 
                        ((= 2 3) 2) 
                        (else 3))
                 env)
               3)
    (write ".")
    (assert-eq (schenne-eval 
                 '(cond ((= 1 3) 1)
                        ((= 1 2) 2)
                        ((= 3 3) 3)
                        (else 4))
                 env) 
               3)
    (write ".")))

(define (test-begin-expression)
  (let ((env (setup-environment)))
    (assert-eq (schenne-eval
                 '(begin )
                 env) unspecified-value)
    (write ".")
    (assert-eq (schenne-eval
                 '(begin (+ 1 1))
                 env) 
               2)
    (write ".")
    (assert-eq (schenne-eval
                 '(begin (+ 1 1) 
                         (+ 2 2))
                 env) 
               4)
    (write ".")
    (assert-eq (schenne-eval
                 '(begin (define x 1) 
                         (set! x 2)
                         (+ 3 3) 
                         (+ x x))
                 env) 
               4)
    (assert-eq (schenne-eval
                 '(begin (define x 1) 
                         (set! x 2)
                         (+ 3 3)
                         (cond ((= x 1) 1) 
                               ((= x 2) 2)
                               (else 3)))
                 env) 
               2)
    (write ".")))
               

(define (run-eval-tests)
  (newline)
  (test-if-expression)
  (write "Passed if-expression tests.")

  (newline)
  (test-definition)
  (write "Passed definition tests.")

  (newline)
  (test-lambda-environment)
  (write "Passed lambda-expression tests.")

  (newline)
  (test-cond-expression)
  (write "Passed cond-expression tests.")

  (newline)
  (test-begin-expression)
  (write "Passed begin-expression tests."))

(run-eval-tests)