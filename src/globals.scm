(define (tagged-with? obj tag)
  (and (pair? obj) (eq? (car obj) tag)))

(define (bool? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)))

(define (false? expr)
  (eq? expr 'false))

(define (true? expr)
  (not (false? expr)))

(define (chain-executables exes)
  (let ((sequentially (lambda (exe1 exe2)
                        (lambda (env)
                          (exe1 env)
                          (exe2 env))))
        (dummy-exe (lambda (env)
                     unspecified-value)))
    (reduce-left sequentially dummy-exe exes)))

