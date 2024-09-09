#lang racket
(require racket/base)
(require threading)

(define odds (list 1 3 5 7 9 11 13 15 17 19 21))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
;;;Ex. 2.17
(define (append-last elem lst)
  (if (null? lst)
      (list elem)
      (cons (car lst) (append-last elem (cdr lst)))))

(define (last-pair lst)
  (list-ref lst (- (length lst) 1)));; less efficient

(define (last-pair-rec lst)
  (if (null? (cdr lst))
      (car lst)
      (last-pair (cdr lst))))
;;;Ex. 2.18;;;]

(define (reverse lst)
  (define (iter lst lst2)
    (if (null? lst)
        lst2
        (iter (cdr lst) (cons (car lst) lst2))))
  (iter lst '()))

(define (rev-cdr-iter lst)
  (define (iter lst lst2)
    (if (null? (cdr lst))
        (reverse lst2)
        (iter (cdr lst) (cons (car lst) lst2))))
  (iter lst '() ))

(define (rev-cdr lst)
  (cond ((null? lst) '())
        ((null? (cdr lst)) '())
        (else (cons (car lst) (rev-cdr (cdr lst))))))
;;;;;;;;;;Ex. 2.19;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (except-first-denomination lst)
  (cdr lst))

(define (first-denomination lst)
  (car lst ))

(define (no-more? lst)
  (null? lst))

;; The order of the list doesn't affect the answer
;; as the combinations remain the same
;;;;;;;;;;;
;;;;;;;;;;;
