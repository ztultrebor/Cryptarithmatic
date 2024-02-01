#lang racket


(define (solve formula)
    """S-Expr -> S-Expr
    Given a formula like 'ODD + ODD == EVEN', fill in digits to solve it.
    Return the solved S-Expr"""
    (ormap valid (fill-in formula)))


(define (fill-in formula)
"""S-Expr -> [ListOf S-Expr]
Generate all possible fillings-in of letters in formula with digits."""
  (local ((define variables (get-lett formula))
          (define replacements (permute '(0 1 2 3 4 5 6 7 8 9) (length variables))))
  ; - IN -
  (map (lambda (r) (substitute formula variables r)) replacements)))


(define (valid f)
    """S-Expr -> [Maybe S-Expr]
    Formula f is valid if and only if it evals true (plus it should have no 
    numbers with leading zeros, but that's not working yet.)"""
    (cond
      [(eval f) f]
      [else #f]))


(define (eval sexpr)
  """S-Expr -> Boolean
  takes an S-expression and converts it into a BSL-expression"""
  (match sexpr
    [(? number?) sexpr]
    [(? boolean?) sexpr]
    [(? symbol?) sexpr]
    [(? string?) (error "no strings allowed")]
    [(list '+ x y) (+ (eval x) (eval y))]
    [(list '* x y) (+ (eval x) (eval y))]
    [(list '= x y) (= (eval x) (eval y))]
    [(list 'and x y) (and (eval x) (eval y))]
    [(list 'or x y) (or (eval x) (eval y))]
    [(list 'not x) (not (eval x))]))


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


(define (substitute formula letters digits)
  """S-Expr [ListOf Char] [ListOf N] -> S-Expr
  takes an S-expression and replaces all alphabetic symbols with numerics"""
  (local ((define (replace l ls ds)
            (cond
              [(empty? ls) #f]
              [(equal? (first ls) l) (number->string (first ds))]
              [else (replace l (rest ls) (rest ds))]))
          (define (substitute formula)
            (match formula
              [(? symbol?) 
                (string->number 
                  (foldr string-append "" 
                                      (map (lambda (l) (replace l letters digits))
                                            (string->list (symbol->string formula)))))]
              [(list '+   x y) (list '+ (substitute x) (substitute y))]
              [(list '=   x y) (list '=  (substitute x) (substitute y))])))
      ; - IN -
      (substitute formula)))

 









(define (permute lst n)
; [ListOf X] N -> [ListOf [ListOf X]]
; create all permutations of given lst that have length n
  (local ((define (permute assemblage parts n)
            (cond
                [(empty? parts) (list assemblage)]
                [(= n 0) (list assemblage)]
                [else
                (foldr append
                        '()
                        (map (lambda (p) (permute (cons p assemblage) (remove p parts) (sub1 n)))
                              parts))])))
    ; - IN -
    (permute '() lst n)))


(define (list->set lst)
  """[ListOf X] -> [ListOf X]
  converts a list into a set, a list in which no items repeat."""
  (local ((define (list->set set lst)
    (cond
      [(empty? lst) set]
      [(member (first lst) (rest lst)) (list->set set (rest lst))]
      [else (list->set (cons (first lst) set) (rest lst))])))
    ; - IN -
    (list->set '() lst)))


(fill-in '(= (+ ODD ODD) EVEN))