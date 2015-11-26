#lang racket
(define (product-digits n)
  (define (iter n x)
    (cond
      [(= n 0) x]
      [else (iter (quotient n 10) (* x (remainder n 10)))]
      )
    )
  (iter n 1)
  )
