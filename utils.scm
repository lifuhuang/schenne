(define (has-tag? obj tag)
  (and (pair? obj) (eq? (car obj tag))))
