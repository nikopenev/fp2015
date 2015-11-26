#lang racket

; Намира сумата на всички числа в numbers
; -> (sum (list))
; 0
; -> (sum (list 1 2 3))
; 6
(define (sum numbers)
  (define (iter n p)
    (cond
      [(empty? n) p]
      [else (iter (cdr n) (+ (car n) p))]
      )
    )
  (iter numbers 0)
  )

; Проверява дали x се среща в items
; -> (member? 1 (list 1 2 3))
; #t
; -> (member? "asdf" (list "asd"))
; #f
; Разгледайте http://docs.racket-lang.org/reference/booleans.html
(define (member? x items)
  (define (iter x str)
    (cond
      [(empty? str) #f]
      [(equal? x (car str)) #t]
      [else (iter x (cdr str))]
      )
    )
  (iter x items)
  )

; -> (length2 (range2 1 10))
; 9
; В Racket има такава функция, наречена length
(define (length2 items)
  (define (iter lis len)
    (cond
      [(empty? lis) len]
      [else (iter (cdr lis) (+ len (string-length (~a (car lis)))))]
      )
    )
  (iter items 0)
  )

; Връща n-тия елемент от items при 0лево базиран индекс
; -> (list-ref2 (list 1 2 3) 0)
; 1
; В Racket има такава функция, наречена list-ref
(define (list-ref2 items n)
  (define (iter list n i)
    (cond
      [(empty? list) "Error"]
      [(= i n) (car list)]
      [(iter (cdr list) n (+ i 1))]
      )
    )
  (iter items n 0)
  )

; -> (range2 1 10)
; '(1 2 3 4 5 6 7 8 9)
; В Racket съществува такава функция, наречена range
(define (range2 a b)
  (define (iter a b lis)
    (cond
      [(> a b) (reverse lis)]
      [else (iter (+ a 1) b (cons a lis))]
      )
    )
  (iter a b '())
  )

; Строи списък от числата между 0 и n, включително, като прилага f върху всяко число
; i-тия елемент на този списък е (f i)
; -> (build-list2 3 id)
; '(0 1 2)
; -> (build-list2 3 (lambda (x) (* x x)))
; '(0 1 4)
; В Racket има такава функция, наречена build-list

(define (build-list2 n f)
  (define (iter a b f res)
    (cond
      [(> a b) (reverse res)]
      [else (iter (+ 1 a) b f (cons (f a) res))]
      )
    )
  (iter 0 n f '())
  )

; Конкатенира два списъка в нов списък
; -> (append2 (list 1 2 3) (list 4 5 6))
; '(1 2 3 4 5 6)
; В Racket има такава фунцкия, наречена append
(define (append2 l1 l2)
  (define (iter a b)
    (cond
      [(empty? a) b]
      [else (iter (cdr a) (cons (car a) b))]
      )
    )
  (iter (reverse l1) l2)
  )

; Обръща списъка наобратно
; -> (reverse2 (list 1 2 3))
; '(3 2 1)
; В Racket има такава функция, наречена reverse
(define (reverse2 items)
  (define (iter str res)
    (cond
      [(empty? str) res]
      [else (iter (cdr str) (cons (car str) res))]
      )
    )
  (iter items '())
  )

; Взима първите n елемента от даден списък
; Ако (> n (length items)), тогава връща items
; -> (take2 3 (list 1 2 3 4 5))
; '(1 2 3)
(define (take2 n items)
  (define (iter str res n i)
    (cond
      [(> n (length str)) str]
      [(= i n) (reverse res)]
      [else (iter (cdr str) (cons (car str) res) n (+ i 1))]
      )
    )
  (iter items '() n 0)
  )

; Маха първите n елемента от даден списък
; Ако (> n (length items)) връща '()
; -> (drop2 3 (list 1 2 3 4 5))
; '(4 5)
(define (drop2 n items)
  (define (iter n i str)
    (cond
      [(= i n) str]
      [else (iter n (+ i 1) (cdr str))]
      )
    )
  (iter n 0 items)
  )

; Функция от по-висок ред. Взима поредни елементи от items докато предиката p за тях дава истина
; -> (take-while zero? (list 0 0 0 1 2 3))
; '(0 0 0)
; -> (take-while even? (list 2 4 5 7 8 3 2))
; '(2 4)
; -> (take-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '()
(define (take-while p items)
  (define (iter p str res)
    (cond
      [(empty? str) res]
      [(not (p (car str))) (reverse res)]
      [else (iter p (cdr str) (cons (car str) res))]
      )
    )
  (iter p items '())
  )

; Функция от по-висок ред. Маха поредните елементи от items докато предикатa p дава лъжа за тях
; -> (drop-while zero? (list 0 0 0 1 2 3))
; '(1 2 3)
; -> (drop-while even? (list 2 4 5 7 8 3 2))
; '(5 7 8 3 2)
; -> (drop-while (lambda (x) (> x 3)) (list 1 1 1 1 1))
; '(1 1 1 1 1)
(define (drop-while p items)
  (define (iter p str res)
    (cond
      [(empty? str) (reverse res)]
      [(not (p (car str))) (iter p (cdr str) (cons (car str) res))]
      [else (iter p (cdr str) res)]
      )
    )
  (iter p items '())
  )

; Функцията взима число и връща списък от цифрите му
; -> (number->list 123)
; '(1 2 3)
(define (number->list n)
  (define (iter n res)
    (cond
      [(<= n 0) res]
      [else (iter (quotient n 10) (append (list (remainder n 10)) res ))]
      )
    )
  (iter n '())
  )

; Функцията взима списък от цифри и връща числото
; -> (list->number (list 1 2 3))
; 123
(define (list->number ns)
  (define (iter lis res pos)
    (cond
      [(empty? lis) res]
      [else (iter (cdr lis) (+ res (* (expt 10 pos) (car lis))) (- pos 1) )]
      )
    )
  (iter ns 0 (- (length ns) 1) )
  )
