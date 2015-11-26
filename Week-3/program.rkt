#lang racket
(require "fence.rkt")
(require "binary.rkt")

(define (beast n) (string-repeat "666" n))

(define (hack n)
  (define (iter n-bin n-pal next)
    (cond
      [(and (equal? n-bin n-pal) (odd-ones n-bin)) next]
      [else (iter (to-binary (+ next 1)) (string-reverse (to-binary (+ next 1))) (+ next 1))]
      )
    )
  (iter (to-binary (+ 1 n)) (string-reverse (to-binary (+ 1 n))) (+ 1 n))
  )

(define (odd-ones str)
  (define (iter str p q n)
    (cond
      [(= p q)
       (cond
         [(= (remainder n 2) 1) #t]
         [(= (remainder n 2) 0) #f]
         )
       ]
      [(equal? (~a(string-ref str p)) "1") (iter str (+ p 1) q (+ n 1) ) ]
      [(equal? (~a(string-ref str p)) "0") (iter str (+ p 1) q n ) ]
      )
    )
  (iter str 0 (string-length str) 0)
  )

(define (invert n) (string->number (string-reverse (number->string n))) )

(define (p-score n)
  (define (iter n q)
    (cond
      [(equal? n (invert n)) (+ 1 q)]
      [else (iter (+ n (invert n)) (+ q 1))]
      )
    )
  (iter n 0)
  )
