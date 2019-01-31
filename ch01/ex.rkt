#lang racket

(define (line) (printf "-------------------------\n"))

;; Ex 1.9
(define (remove-all target l)
  (cond
    [(null? l) '()]
    [(eqv? target (car l)) (remove-all target (cdr l))]
    [else (cons (car l) (remove-all target (cdr l)))]))

(remove-all '3 '(1 2 3 3 4 3 3 5))
(remove-all 3 '(1 2 3 3 4 3 3 5 3))
(remove-all "3" '(1 2 3 3 4 3 3 5 3))

;; Ex 1.15
(define (duple n x)
  (define (impl r n x)
    (if (zero? n)
        r
        (if (pair? x)
            (impl (append r (list x)) (- n 1) x)
            (impl (cons x r) (- n 1) x))))
  (impl '() n x))

(duple 2 3)
(duple 4 '(ha ha))
(duple 0 '(blah))

(line)

;; Ex 1.16
(define (invert lst)
  (define (impl l)
    (if (pair? l)
      (reverse (map impl l))
      l))
  (reverse (impl lst)))
(invert '((a 1) (a 2) (1 b) (2 b)))

(line)

;; Ex 1.17
(define (down lst)
  (map (λ (x)
         (if (not (pair? x))
             (list x)
             (down x))) lst))
(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))

(line)

;; Ex 1.18
(define (swapper s1 s2 slist)
  (map (λ (x)
         (if (symbol? x)
             (cond
               [(eqv? x s1) s2]
               [(eqv? x s2) s1]
               [else x])
             (swapper s1 s2 x))) slist))
(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))

(line)

;; Ex 1.19
(define (list-set lst n x)
  (if (or (>= n (length lst)) (< n 0))
      (error "index out of range" n)
      (map (λ (v)
         (let ([y n])
           (set! n (- n 1))
           (if (= y 0) x v))) lst)))

(define (list-set2 lst n x)
  (define (impl nl lst n x)
    (if (= n 0)
        (append nl (cons x (cdr lst)))
        (impl (append nl (list (car lst))) (cdr lst) (- n 1) x)))
  (if (or (>= n (length lst)) (< n 0))
      (error "index out of range" n)
      (impl '() lst n x)))

(list-set '(a b c d) 3 '(1 2))
(list-set2 '(a b c d) 3 '(1 2))
(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
(list-ref (list-set2 '(a b c d) 3 '(1 5 10)) 3)

(line)

;; Ex 1.20
(define (count-occurrences s slist)
  (define n 0)
  (define (impl s slist)
    (map (λ (x)
         (if (symbol? x)
             (if (eqv? x s)
                 (begin
                   (set! n (+ n 1))
                   x)
                 x)
             (impl s x))) slist))
  (impl s slist)
  n)

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))

(line)

;; Ex 1.21
(define (product sos1 sos2)
  (define (iter n l res)
    (if (null? l)
        res
        (iter n (cdr l) (append res (list (list n (car l)))))))
  (define (iter2 l res)
    (if (null? l)
        res
        (iter2 (cdr l) (append (iter (car l) sos2 res)))))
  (iter2 sos1 '()))

(product '(a b c ) '(x y))
(product '(x y) '(a b c))

(line)

;; Ex 1.22
(define (filter-in pred lst)
  (define (impl res l)
    (if (null? l)
        res
        (impl (if (pred (car l))
                  (append res (list (car l)))
                  res) (cdr l))))
  (impl '() lst))
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))

(line)

;; Ex 1.23
(define (list-index pred lst)
  (define (impl n l)
    (if (null? l)
        #f
        (if (pred (car l))
            n
            (impl (+ n 1) (cdr l)))))
  (impl 0 lst))
(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))

(line)

;; Ex 1.24
(define (every? pred lst)
  (if (null? lst)
      #t
      (if (pred (car lst))
          (every? pred (cdr lst))
          #f)))
(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))

(line)

;; Ex 1.25
(define (exists? pred lst)
  (if (null? lst)
      #f
      (if (pred (car lst))
          #t
          (exists? pred (cdr lst)))))
(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))

(line)

;; Ex 1.26
(define (up lst)
  (cond
    [(null? lst) '()]
    [(list? (car lst)) (append (car lst) (up (cdr lst)))]
    [else (cons (car lst) (up (cdr lst)))]))

(up '((1 2) (3 4)))
(printf "up-down & down-up\n")
(up (down '((1 2) (3 4))))
(down (up '((1 2) (3 4))))

(line)

;; Ex 1.27
(define (flatten lst)
  (cond
    [(null? lst) '()]
    [(not (pair? lst)) (list lst)]
    [else (append (flatten (car lst)) (flatten (cdr lst)))]))
(flatten '((a) () (b ()) () (c)))
(flatten '(1 2 3))

(line)

;; Ex 1.28
;; loi => list of integer
(define (merge loi1 loi2)
  (cond
    [(null? loi1) loi2]
    [(null? loi2) loi1]
    [(< (car loi1) (car loi2))
     (cons (car loi1) (merge (cdr loi1) loi2))]
    [else
     (cons (car loi2) (merge loi1 (cdr loi2)))]))
(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))

(line)

;; Ex 1.29
(define (sort loi)
  (if (null? loi)
      '()
      (append (sort (filter-in (λ (x) (> (car loi) x)) (cdr loi)))
              (list (car loi))
              (sort (filter-in (λ (x) (not (> (car loi) x))) (cdr loi))))))

(sort '(8 2 5 2 3))

(line)

;; Ex 1.30
(define (sort/pred pred loi)
  (if (null? loi)
      '()
      (append (sort/pred pred (filter-in (λ (x) (pred (car loi) x)) (cdr loi)))
              (list (car loi))
              (sort/pred pred (filter-in (λ (x) (not (pred (car loi) x))) (cdr loi))))))
(sort/pred < '(8 2 5 2 3))
(sort/pred > '(8 2 5 2 3))

;; Ex 1.31 ~ 1.36 seems like binary-mobile in SICP, skip.