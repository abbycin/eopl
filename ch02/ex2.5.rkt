#lang racket

(define empty-env
  (λ () '()))

(define extend-env
  (λ (k v env)
    (cons
     (cons k v) env)))

(define apply-env
  (λ (env search-var)
    (cond
     ((null? env)
      (report-no-binding-found search-var))
     ((eqv? (caar env) search-var)
      (cdr (car env)))
     (else
      (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (λ (search-var)
    (error 'apply-env "No binding for: " search-var)))

(define e
  (extend-env 'd 6
    (extend-env 'y 8
       (extend-env 'x 7
	  (extend-env 'y 14
	    (empty-env))))))

(printf "~a\n" e)
(= (apply-env e 'd) 6)
(= (apply-env e 'y) 8)
(= (apply-env e 'x) 7)