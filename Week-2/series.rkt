#lang racket
(define (series a b n)
    (cond
      [(= n 1) a]
      [(= n 2) b]
      [else (series b (+ b a) (- n 1))]
      )
  )

(define (lucas n) (series 2 1 n))

(define (fibonacci n) (series 1 1 n))

(define (summed-member n) (+ (lucas n) (fibonacci n)) )

(define (nth-fib n)
  (define (iter n p q)
    (cond
      [(> p n) q]
      [else (iter n (+ p 1) (+ q (fibonacci p)))]
      )
    )
  (iter n 1 0)
  )

(define (nth-luc n)
  (define (iter n p q)
    (cond
      [(> p n) q]
      [else (iter n (+ p 1) (+ q (lucas p)))]
      )
    )
  (iter n 1 0)
  )

(define (lcdiff n) (- (lucas n) (fibonacci n)))
