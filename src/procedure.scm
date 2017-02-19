(load "src/utils.scm")

;; Primitive procedure

(define primitive-tag 'primitive)

(define (primitive-procedure? p)
  (tagged-with? p primitive-tag))

(define (make-primitive-procedure imp)
  (list (primitive-tag imp)))

(define (primitive-procedure-implementation p)
  (cadr p))

(define (apply-primitive-procedure p args)
  (apply (cadr p) args))


;; Compound procedure

(define compound-tag 'compound)

(define (compound-procedure? p)
  (tagged-with? p compound-tag))

(define (make-compound-procedure parameters body env)
  (list compound-tag parameters body env))

(define (compound-procedure-parameters p)
  (cadr p))

(define (compound-procedure-body p)
  (caddr p))

(define (compound-procedure-env p)
  (cadddr p))

(define (apply-compound-procedure p args)
  (let ((frame (make-frame 
                 (zip (compound-procedure-parameters p) args))))
    (schenne-eval (compound-procedure-body p)
                  (extend-environment (compound-procedure-env p) frame))))
