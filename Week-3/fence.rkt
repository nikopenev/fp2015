#lang racket
(define (string-repeat str n)
  (define (iter str n fin)
    (cond
      [(= n 0) fin]
      [(iter str (- n 1) (string-append fin str))]
      )
    )
  (iter str n "")
  )

(define (fence n)
  (define (iter n p q fin)
    (cond
      [(= p q) (string-append "{" (string-append fin "}"))]
      [(iter n (+ p 1) q (string-append "-" (string-append fin "-")))]
      )
    )
  (iter n 0 (round(+ 1 (log n))) (string-append ">" (string-append (number->string n) "<")))
  )

(provide string-repeat)
