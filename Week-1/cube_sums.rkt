#lang racket
(define (cube-sums? n)
  (define (iter n p q)
    (cond
      [(= (+ (* p p p) (* q q q)) n) #t]
      [(and (> p (expt n 1/3)) (< q (expt n 1/3))) (iter n p (+ q 1))]
      [(= q 1) (iter n (+ p 1) q)]
      [else #f]
      )
    )
  (iter n 1 1)
  )
