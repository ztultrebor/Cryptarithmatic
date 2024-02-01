#lang racket

(require rackunit)


; =======================
; functions


(define (solve formula)
  """S-Expr -> S-Expr
     Given a formula like 'ODD + ODD == EVEN', fill in digits to solve it.
     Return the solved S-Expr"""
  (filter (lambda (f) (not (false? f))) (map valid (fill-in formula))))


; !!! fix leading 0 problem
(define (valid f)
  """S-Expr -> [Maybe S-Expr]
     Formula f is valid if and only if it evals true (plus it should have no 
     numbers with leading zeros, but that's not working yet.)"""
  (cond
    [(eval f) f]
    [else #f]))


(define (fill-in formula)
  """S-Expr -> [ListOf S-Expr]
     Generate all possible fillings-in of letters in formula with digits."""
  (local ((define variables (get-lett formula))
          (define replacements (permute '(0 1 2 3 4 5 6 7 8 9) (length variables))))
    ; - IN -
    (map (lambda (r) (substitute formula variables r)) replacements)))


; !!! there must be a better--an abstract--way
(define (eval sexpr)
  """S-Expr -> Boolean
     takes an S-expression and mathematically evaluates it for truth"""
  (local ((define (is? op vars)
            (local ((define evarls (map eval vars)))
              (andmap op (rest (reverse evarls)) (reverse (rest evarls))))))
    (match sexpr
      [(? number?) sexpr]
      [(? boolean?) sexpr]
      [(? symbol?) sexpr]
      [(? string?) (error "no strings allowed")]
      [(cons '+ vars) (foldl + 0 (map eval vars))]
      [(cons '* vars) (foldl * 1 (map eval vars))]
      [(cons 'and vars) (andmap eval vars)]
      [(cons 'or vars) (ormap eval vars)]
      [(cons '= vars) (is? = vars)]
      [(cons '> vars) (is? > vars)]
      [(cons '< vars) (is? < vars)]
      [(list 'not x) (not (eval x))]
      [(list 'sqrt x) (sqrt (eval x))]
      [(list 'expt x n) (expt (eval x) (eval n))])))


(define (get-lett formula)
  """S-Expr -> [ListOf Char]
     takes an S-expression and extracts all the 'variables'"""
  (local ((define (get-lett formula)
            (match formula
              [(? number?) ""]
              [(? symbol?) (symbol->string formula)]
              [(cons op vars) (foldr string-append "" (map get-lett vars))])))
    ; - IN -
    (list->set (string->list (get-lett formula)))))


(define (substitute formula letters digits)
  """S-Expr [ListOf Char] [ListOf N] -> S-Expr
     takes an S-expression and replaces all alphabetic letters with numeric digits"""
  (local ((define (replace l ls ds)
            ; Char [ListOf Char] [ListOf N] -> Char
            (cond
              [(empty? ls) #f]
              [(equal? (first ls) l) (number->string (first ds))]
              [else (replace l (rest ls) (rest ds))]))
          (define (substitute formula)
            ; S-Expr -> S-Expr
            (match formula
              [(? number?) formula]
              [(? symbol?) 
               (string->number 
                (foldr string-append "" 
                       (map (lambda (l) (replace l letters digits))
                            (string->list (symbol->string formula)))))]
              [(cons op vars) (cons op (map substitute vars))])))
    ; - IN -
    (substitute formula)))

 
(define (permute lst n)
  """[ListOf X] N -> [ListOf [ListOf X]]
     create all permutations of given lst that have length n"""
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



; =====================
; checks


(check-equal? (list->set '(1 2 3 3 4 5 3 4 5)) '(5 4 3 2 1))
(check-equal? (permute '(1 2 3) 2) '((2 1) (3 1) (1 2) (3 2) (1 3) (2 3)))
(check-equal? (get-lett '(= (+ ODD ODD) EVEN)) '(#\N #\E #\V #\D #\O))
(check-equal? (substitute '(= (+ ODD ODD) EVEN) '(#\N #\E #\V #\D #\O) '(5 4 3 2 1))
              '(= (+ 122 122) 4345))
(check-equal? (substitute '(= (sqrt ATOM) (+ AT OM)) '(#\A #\T #\O #\M) '(9 8 0 1))
              '(= (sqrt 9801) (+ 98 01)))
(check-equal? (fill-in '(= U 0)) '((= 0 0) (= 1 0) (= 2 0) (= 3 0) (= 4 0) (= 5 0)
                                           (= 6 0) (= 7 0) (= 8 0) (= 9 0)))
(check-equal? (valid '(= (+ 122 122) 4345)) #f)
;(check-equal? (valid '(= (sqrt 9801) (+ 98 01))) #f) ; should be #f
(check-equal? (eval '(= (+ 655 655) 1310)) #t)
(check-equal? (eval '(= (+ 122 122) 4345)) #f)
(check-equal? (eval '(= (+ 655 655) 1310)) #t)
(check-equal? (solve '(= (+ ODD ODD) EVEN)) '((= (+ 655 655) 1310) (= (+ 855 855) 1710)))


; =====================
; actions!


(solve '(= (+ ODD ODD) EVEN))
(solve '(= (sqrt ATOM) (+ A TO M)))
(solve '(= (sqrt ATOM) (+ AT O M)))
(solve '(= (sqrt ATOM) (+ AT OM)))
(solve '(and (= (+ (expt A N) (expt B N)) (expt C N)) (> N 1)))
(solve '(and (= (+ (expt A N) (expt IB N)) (+ (expt I N) (expt ID N)) IXDA) (> N 1)))