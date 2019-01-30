#lang racket

;; Exercise 1.13

;; Version 1 Mutual Recursion
;; subst: Sym x Sym x S-list -> S-list
(define (subst new old slist)
  (define (impl)
    (if (null? slist)
        '()
        (cons (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist)))))
  (impl))

;; subst-in-s-exp: Sym x Sym x S-exp -> S-exp
(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp) ;; atom
      (if (eqv? old sexp)
          new
          sexp)
      (subst new old sexp)))

;; Version 2 inlining (mutual recursion)
(define (subst2 new old slist)
  (define (impl2 sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp) new sexp)
        (impl sexp)))
  (define (impl l)
    (if (null? l)
        '()
        (cons (impl2 (car l))
              (impl (cdr l)))))
  (impl slist))

;; Version 3 following the original grammar by using map
(define (subst3 new old slist)
  (map (λ (x)
         (if (symbol? x)
             (if (eqv? x old) new x)
             (subst3 new old x))) slist))

(define slist '((b c) (b () d)))
(subst 'a 'b slist)
(subst2 'a 'b slist)
(subst3 'a 'b slist)
