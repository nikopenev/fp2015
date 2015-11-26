#lang racket

(define (group lis)
  (define (iter lis res buff last)
    (cond
      [(empty? lis) (cond
                      [(empty? res) (list buff) ]
                      [(equal? last (car (list-ref res (- (length res) 1) ) ) )   (append res (list buff))]
                      [else (append res (list buff)) ] )
       ]
      [else (iter (cdr lis) (cond
                              [(equal? (car lis) last) res]
                              [else (append res (list buff)) ]
                              )
                            (cond
                              [(equal? (car lis) last) (append buff (list last)) ]
                              [else (list (car lis))]
                              )
                            (car lis) ) ]
    )
    )
  (cond
    [(empty? lis) lis]
    [else (iter (cdr lis) '() (list (car lis)) (car lis)) ]
    )
  )

(define (encode str)
  (define (list->string1 lis)
    (define (iter lis counter symbol)
      (cond
        [(empty? lis) (string-append (number->string counter) symbol)]
        [else (iter (cdr lis) (+ counter 1) symbol)]
        )
      )
    (cond
      [(= (length lis) 1) (~a (car lis))]
      [else (iter lis 0 (~a (car lis)))]
      )
    )
  
  (define (string->list1 str)
    (define (iter str res count)
      (cond
        [(equal? count (string-length str)) reverse res]
        [else (iter str (append res (list (string-ref str count)) ) (+ count 1)) ]
        )
      )
    (iter str '() 0)
    )
  
  (define (list->conv lis)
    (define (iter lis res)
      (cond
        [(empty? lis) res]
        [else (iter (cdr lis) (string-append res (list->string1 (car lis)) )) ]
        )
      )
    (iter lis "")
    )
  (list->conv (group (string->list1 str)))
  )
  
