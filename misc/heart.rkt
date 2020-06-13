#lang racket

(require plot)

(define (heart [n 5.20])
  (define (range r f)
    (if (< (- n f) 0.01)
        r
        (range (cons f r) (+ f 0.01))))
  (define ps (range '() (- n)))
  (define xs
    (λ (t) (* 16 (expt (sin t) 3))))
  (define ys
    (λ (t) (- (* 13 (cos t)) (* 5 (cos (* 2 t))) (* 2 (cos (* 3 t))) (cos (* 4 t)))))
  (define (points r fp ns)
    (map (λ (t)
           (append r (fp t)))
         ns))
  (plot (lines-interval
         (map vector
              (points '() xs ps)
              (points '() ys ps))
         '(#(0 1))
         #:color 'red
         #:line1-color 'red)))

(heart)