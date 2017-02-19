(define (tagged-with? obj tag)
  (and (pair? obj) (eq? (car obj tag))))
