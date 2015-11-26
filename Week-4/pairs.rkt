#lang racket
(define (f p g h)
  (lambda (x) (and (p (g x)) (p (h x)) ) )
  )

(define (add-frac f1 f2)
  (cond
    [(equal? (cdr f1) (cdr f2)) cons( (+ (car f1) (car f2)) (cdr f1))]
    [else (cons (+ (* (car f1) (cdr f2)) (* (car f2) (cdr f1))) (lcm (cdr f1) (cdr f2)) )]
    )
  )

(define (sub-frac f1 f2)
  (cond
    [(equal? (cdr f1) (cdr f2)) cons( (- (car f1) (car f2)) (cdr f1))]
    [else (cons (- (* (car f1) (cdr f2)) (* (car f2) (cdr f1))) (lcm (cdr f1) (cdr f2)) )]
    )
  )

(define (mult-frac f1 f2) (cons (* (car f1) (car f2) ) (* (cdr f1) (cdr f2) ) ) )

(define (simp-frac f)
  (cond
    [(= (gcd (car f) (cdr f)) 1) f]
    [else (simp-frac (cons (/ (car f) (gcd (car f) (cdr f)) ) (/ (cdr f) (gcd (car f) (cdr f)) ) ) )]
    )
  )
