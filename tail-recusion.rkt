#lang racket
(require racket/base)
(require threading)

(define numLst '(1 2 3 4 5 6 7 8 9 10))
(define (factorial n)
  (define (iter a result)
         (if (= a 0)
             result
             (iter (- a 1) (* a result))))
  (iter n 1))

(define (fibonacci n)
  (define (iter k a b)
    (if (= k n)
        a
        (iter (+ k 1) b (+ a b))))
  (iter 0 0 1))

(define (sum-list lst)
  (define (iter lst result)
    (if (null? lst)
        result
        (iter (cdr lst) (+ (car lst) result))))
  (iter lst 0))

(define (gcd a b)
  (define (iter a b)
    (if (= b 0)
        a
        (iter b (modulo a b))))
  (iter a b))
;;; to-do funcitions
(define (power base exp)
  (define (iter i result)
    (if (= i 0)
     result
      (iter (- i 1) (* base result))))
  (iter exp 1))

(define (reverse-list lst)
  (define (iter new-list lst)
    (if (null? lst)
        new-list
        (iter (cons (car lst) new-list) (cdr lst))))
  (iter '() lst))


(define (binary-search lst target)
  (define (search low high)
    (if (> low high)
        -1
        (let ((mid (quotient (+ low high) 2)))
          ;; (displayln (format "mid: ~a" mid))
          (cond
            ((= (list-ref lst mid) target) mid)
            ((< (list-ref lst mid) target) (search (+ mid 1) high))
            (else (search low (- mid 1)))))))
  (search 0 (- (length lst) 1)))

(define (binary-search-iter lst target)
  (define (search low high)
    (displayln (format "recursive call" ))
    (let loop ((low low) (high high))
      (if (> low high)
          -1
          (let ((mid (quotient (+ low high) 2)))
            ;; (displayln (format "mid: ~a" mid))
            (cond
              ((= (list-ref lst mid) target) mid)
              ((< (list-ref lst mid) target) (loop (+ mid 1) high))
              (else (loop low (- mid 1))))))))
  (search 0 (- (length lst) 1)))
