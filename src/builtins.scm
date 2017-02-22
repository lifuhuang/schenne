(define (LT x y)
  (if (< x y)
    'true
    'false))

(define (GT x y)
  (if (> x y)
    'true
    'false))

(define (LE x y)
  (if (<= x y)
    'true
    'false))

(define (GE x y)
  (if (>= x y)
    'true
    'false))

(define (EQ x y)
  (if (= x y)
    'true
    'false))

(define (SQUARE x)
  (* x x))

(define (CUBE x)
  (* x x x))

(define (INC x)
  (+ x 1))

(define (DEC x)
  (- x 1))

(define builtin-list
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '/ /)
        (cons '= EQ)
        (cons '< LT)
        (cons '> GT)
        (cons '<= LE)
        (cons '>= GE)
        (cons 'square SQUARE)
        (cons 'cube CUBE)
        (cons 'inc INC)
        (cons 'dec DEC)
        (cons 'cons cons)
        (cons 'car car)
        (cons 'cdr cdr)
        (cons 'list list)))

