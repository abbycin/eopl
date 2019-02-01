#lang racket

;; constructors
(define BASE 16)

(define zero '())

(define is-zero?
  (λ (n)
    (null? n)))

(define (successor n)
  (cond [(null? n) '(1)]
        [(= (+ (car n) 1) BASE) (cons 0 (successor (cdr n)))]
        [else (cons (+ (car n) 1) (cdr n))]))

(define (predecessor n)
  (cond [(null? n) (error "result is not natural number")]
        [(zero? (car n)) (cons (- BASE 1) (predecessor (cdr n)))]
        [(and (= (car n) 1) (null? (cdr n))) zero]
        [else (cons (- (car n) 1) (cdr n))]))

;; client
(define (bignum n)
  (define (impl m res)
    (if (= m 0)
        res
        (impl (- m 1) (successor res))))
  (impl n zero))

(define (plus n m)
  (if (is-zero? m)
      n
      (plus (successor n) (predecessor m))))

(define (mul n m)
  (define (impl fix n m)
    (if (is-zero? m)
        n
        (impl fix (plus n fix) (predecessor m))))
  (if (or (is-zero? n) (is-zero? m))
      zero
      (impl n n (predecessor m))))

(define (factorial n)
  (if (is-zero? n)
      (successor zero)
      (mul n (factorial (predecessor n)))))

(define (factorial2 n)
  (define (impl m res)
    (if (is-zero? m)
        res
        (impl (predecessor m) (mul res m))))
  (impl n (successor zero)))
      
(factorial (bignum 10))
(factorial2 (bignum 10))
(bignum 3628800)