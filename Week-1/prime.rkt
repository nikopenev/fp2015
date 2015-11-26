#lang racket
(define (prime? n)
  (define (iter n q)
    (cond
      [(> q (quotient n 2)) #t]
      [(not (= (remainder n q) 0)) (iter n (+ q 1))]
      [else #f]
      )
    )
  (iter n 2)
  )
