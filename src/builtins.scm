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

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define builtin-list
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= =)
        (cons '< lt)
        (cons '> gt)
        (cons '<= le)
        (cons '>= ge)
        (cons 'square square)
        (cons 'cube cube)
        (cons 'inc inc)
        (cons 'dec dec)
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'list list)))

