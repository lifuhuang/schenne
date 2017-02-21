(load "src/core.scm")

(define (run-repl)
  (let ((env (setup-environment)))
    (define (loop)
      (newline)
      (write ">>")
      (write (schenne-eval (read) env))
      (loop))
    (loop)))

(run-repl)

