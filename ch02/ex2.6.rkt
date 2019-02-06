#lang racket

(define (report-no-binding-found k)
  (printf "~a is not exist\n" k))

(define (assert v . r)
  (if (not v)
      (error "assert failed" r)
      '()))

;; first
(define (test1)
(define (empty-env) '())
  
(define (extend-env k v env)
  (cons (cons k v) env))
  
(define (apply-env env k)
  (cond [(null? env) (report-no-binding-found k)]
        [(eqv? (caar env) k) (cdar env)]
        [else (apply-env (cdr env) k)]))
  
(define env (extend-env 'd 6 (extend-env 'y 8 (extend-env 'x 7 (extend-env 'y 14 (empty-env))))))
(display env)
(newline)
  
(assert (= (apply-env env 'x) 7) "x == 7")
(assert (= (apply-env env 'y) 8) "y == 8")
(assert (= (apply-env env 'd) 6) "d == 6")
(apply-env env 'h))

(test1)

;; second
(define (test2)
(define (empty-env) '(() ()))
  
(define (extend-env k v env)
  (list (cons k (car env)) (cons v (cadr env))))
  
(define (apply-env env k)
  (cond [(null? (car env)) (report-no-binding-found k)]
        [(eqv? (caar env) k) (caadr env)]
        [else (apply-env (list (cdar env) (cdadr env)) k)]))
  
(define env (extend-env 'd 6 (extend-env 'y 8 (extend-env 'x 7 (extend-env 'y 14 (empty-env))))))
(display env)
(newline)
  
(assert (= (apply-env env 'x) 7) "x == 7")
(assert (= (apply-env env 'y) 8) "y == 8")
(assert (= (apply-env env 'd) 6) "d == 6")
(apply-env env 'h))

(test2)

;; third
;; ((d 6) (y 8 14) (x 7))
;; empty env is '()