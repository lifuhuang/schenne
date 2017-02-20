(load "src/globals.scm")

;; Primitive procedure

(define primitive-tag 'primitive)

(define (primitive-proc? p)
  (tagged-with? p primitive-tag))

(define (make-primitive-proc imp)
  (list primitive-tag imp))

(define (primitive-proc-implementation p)
  (cadr p))


;; Compound procedure

(define compound-tag 'compound)

(define (compound-proc? p)
  (tagged-with? p compound-tag))

(define (make-compound-proc parameters body env)
  (list compound-tag parameters body env))

(define (compound-proc-parameters p)
  (cadr p))

(define (compound-proc-body p)
  (caddr p))

(define (compound-proc-env p)
  (cadddr p))

