#lang racket
(define (string-reverse str)
  (define (iter str fin p)
    (cond
      [(< p 0) fin]
      [(iter str (string-append fin (~a (string-ref str p))) (- p 1))]
      )
    )
  (iter str "" (- (string-length str) 1))
  )

(define (to-binary n)
  (define (iter n fin)
    (cond
      [(< n 1) fin]
      [else (iter (quotient n 2) (string-append (number->string (remainder n 2)) fin))]
      )
    )
  (cond
    [(= n 0) "0"]
    [else (iter n "")]
    )
  )

(define (from-binary str)
  (define (iter str fin p q)
    (cond
      [(= p q) fin]
      [else (iter str (+ fin (* (string->number (~a (string-ref str p))) (expt 2 (- q p 1)) ) ) (+ p 1) q)]
      )
    )
  (iter str 0 0 (string-length str))
  )

(provide to-binary from-binary string-reverse)
