(load "test.scm")
(load "environment.scm")

(define (test-frame)
  (define frame1 (make-frame))
  (add-binding-to-frame! frame1 'x 10)
  (add-binding-to-frame! frame1 'y 20)
  (add-binding-to-frame! frame1 'z 30)
  (assert-equal frame1 '(Head (z . 30) (y . 20) (x . 10))))

(define (test-environment)
  (define frame1 (make-frame))
  (add-binding-to-frame! frame1 'x 10)
  (add-binding-to-frame! frame1 'y 20)
  (add-binding-to-frame! frame1 'z 30)
  (define inner-env (extend-environment the-empty-environment frame1))
  (define outer-env (extend-environment inner-env (make-frame)))

  (assert-eqv (find-variable 'x outer-env) 10)
  (assert-eqv (find-variable 'y outer-env) 20)
  (assert-eqv (find-variable 'z outer-env) 30)

  (set-variable! 'x 200 outer-env)
  (assert-eqv (find-variable 'x inner-env) 200)
  (assert-eqv (find-variable 'x outer-env) 200)

  (define-variable! 'x 300 outer-env)
  (assert-eqv (find-variable 'x inner-env) 200)
  (assert-eqv (find-variable 'x outer-env) 300))

