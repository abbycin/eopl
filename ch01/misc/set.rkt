#lang racket

;;1, 一个自然数 n 在集合 S 中，当且仅当 n = 0 或 n - 3 属于 S
;;2, 一个集合 S 有 N 个元素，且满足 0 属于 S 并且 如果 n 属于 S 那么 n + 3 也属于 S
;;3, 找到一个自然数的集合 T 有 0 属于 T 且 对于 n 属于 T 那么 n + 3 也属于 T, 但 T 不等于 2 中的 S

(define (true) (display "true") (newline))
(define (false) (display "false") (newline))
;; 1, top down
(define (in-set1 number)
  (if (zero? number)
      (true)
      (if (>= (- number 3) 0)
          (in-set1 (- number 3))
          (false))))

(define (set-impl s n l)
    (if (zero? n)
        (true)
        (cond
          [(< s n) (set-impl (+ s 3) n (- l 1))]
          [(and (= s n) (not (zero? l))) (true)]
          [else (false)])))
;; 2
(define (in-set2 number size)
  (set-impl 0 (abs number) size))

;; 3, bottom up
(define (in-set3 number size)
  (set-impl 0 number size))

(in-set1 4)
(in-set2 -3 100)
(in-set3 -3 100)
