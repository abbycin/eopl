#lang racket

(define empty-stack
  (λ () '()))

(define push
  (λ (x s) (cons x s)))

(define pop
  (λ (s)
    (cdr s)))

(define top
  (λ (s) (list-ref s 0)))

(define empty-stack?
  (λ (s) (null? s)))

(define l (push 1 (empty-stack)))
(empty-stack? l)