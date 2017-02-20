(define (tagged-with? obj tag)
  (and (pair? obj) (eq? (car obj) tag)))

(define (bool? expr)
  (or (eq? expr 'true)
      (eq? expr 'false)))

(define (false? expr)
  (eq? expr 'false))

(define (true? expr)
  (not (false? expr)))

