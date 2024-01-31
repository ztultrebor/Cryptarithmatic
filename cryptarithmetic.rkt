#lang racket


(define (parse sexpr)
  ; S-Expr -> BSL-Expr
  ; takes an S-expression and converts it into a BSL-expression
  (match sexpr
    [(? number?) sexpr]
    [(? boolean?) sexpr]
    [(? symbol?) sexpr]
    [(? string?) (error "no strings allowed")]
    [(list '+ x y) (+ (parse x) (parse y))]
    [(list '* x y) (+ (parse x) (parse y))]
    [(list '= x y) (= (parse x) (parse y))]
    [(list 'and x y) (and (parse x) (parse y))]
    [(list 'or x y) (or (parse x) (parse y))]
    [(list 'not x) (not (parse x))]))


(define (get-lett formula)
  ; S-Expr -> BSL-Expr
  ; takes an S-expression and converts it into a BSL-expression
  (local ((define (get-lett formula)
    (match formula
      [(? symbol?) (symbol->string formula)]
      [(list '+   x y) (string-append (get-lett x) (get-lett y))]
      [(list '=   x y) (string-append (get-lett x) (get-lett y))])))
    ; - IN -
    (list->set (string->list (get-lett formula)))))


(define (substitute formula digits)
  """S-Expr [ListOf N] -> S-Expr
  takes an S-expression and replaces all alphabetic symbols with numerics"""
  (local ((define letters (get-lett formula))
          (define (replace l ls ds)
            (cond
              [(empty? ls) #f]
              [(equal? (first ls) l) (number->string (first ds))]
              [else (replace l (rest ls) (rest ds))]))
          (define (substitute formula)
            (match formula
              [(? symbol?) (string->symbol (foldr string-append "" (map (lambda (l) (replace l letters digits))
                              (string->list (symbol->string formula)))))]
              [(list '+   x y) (list '+ (substitute x) (substitute y))]
              [(list '=   x y) (list '=  (substitute x) (substitute y))])))
      ; - IN -
      (substitute formula)))

 
(define (solve formula)
    """Given a formula like 'ODD + ODD == EVEN', fill in digits to solve it.
    Input formula is a string; output is a digit-filled-in string or None."""
    (ormap valid (fill_in formula)))


; !!!
(define (fill_in formula)
"""Generate all possible fillings-in of letters in formula with digits."""
  
  ; - IN -
  (list formula))


(define (valid f)
    """Formula f is valid if and only if it has no 
    numbers with leading zero, and evals true."""
    (cond
      [(parse f) f]
      [else #f]))


(define (list->set lst)
  ; [ListOf X] -> [ListOf X]
  ; converts a list into a set, a list in which no items repeat.
  (local ((define (list->set set lst)
    (cond
      [(empty? lst) set]
      [(member (first lst) (rest lst)) (list->set set (rest lst))]
      [else (list->set (cons (first lst) set) (rest lst))])))
    ; - IN -
    (list->set '() lst)))



(define subby (substitute '(= (+ ODD ODD) EVEN) '(0 1 2 3 4 5 6 7 8 9)))

(parse subby)