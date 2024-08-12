#lang racket
(require racket/base)
(require threading)

(define ls '(2 4 6))
(define (sumLst lst)
  (if (empty? lst)
      0
      (+
       (first lst)
       (sumLst (rest lst)))))

(define (sumTR lst)
  (define (sumLstTail lst acc)
    (if (empty? lst)
      acc
      (sumLstTail(rest lst) (* acc (first lst)))))
  (sumLstTail lst 1))
;;;;;;;;;;;; square-root procedure ;;;;;;;;;;;;;
(define (sqrt-iter guess x)
  (displayln (format "Current guess: ~a" guess))
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
   (< (abs (- (square guess) x)) (* x 0.001)))
      ;(< (abs (- guess prev-guess)) 0.001)))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;cube-root procedure;;;;;;;;;;;;;;;;;;

(define (cbrt x)

  (define (cube x)
    (* x x x))
  (define (cube-enough? guess x)
    (< (abs (- (cube guess) x)) (* x 0.001)))
  (define (cimprove guess x)
      (/ (+ (/ x (sqr guess)) (* 2 guess)) 3))
  (define (cube-iter guess x)
    (displayln (format "Current guess: ~a" guess))
    (if (cube-enough? guess x)
        guess
        (cube-iter (cimprove guess x) x)))
  (cube-iter 1.0 x))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (double x) (+ x x))
(define (sqr x) (exp (double (log x))))

;; Ackerman's function
(define (A x y)
(cond ((= y 0) 0)
((= x 0) (* 2 y))
((= y 1) 2)
(else (A (- x 1) (A x (- y 1))))))
;; change
(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
  ((or (< amount 0) (= kinds-of-coins 0)) 0)
  (else (+ (cc amount
               (- kinds-of-coins 1))
           (cc (- amount
                  (first-denomination
                   kinds-of-coins))
               kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
;;;;;;;Excercise 1.11;;;;;;;;;;;;;;
;; recursive
(define (fn x)
  (cond ((< x 3) x)
        (else(+ (fn (- x 1))
                (* (fn (- x 2)) 2)
                (* (fn (- x 3)) 3)))))
;; iterative
(define (fn2 x)
  (fn-iter 0 1 2 x))

(define (fn-iter a b c acc)
  (if (= acc 0)
      a
      (fn-iter b c
               (+ c
                  (* 2 b)
                  (* 3 a))
                  (- acc 1))))

(define (fact n)
  (~> (range 1 (+ n 1))    ; Generate a list from 1 to n
      (foldl * 1 _)))
;; (define (fact n)
  ;; (foldl * 1 (range 1 (+ n 1))))
;;;; pascal triangle
(define (pascal n k)
  (if (= k 0)
    1
  (bin-the (n k))))

(define (bin-the n k)
    (/ (fact n) (*(fact k) (fact (- n k)))))

;; (define (pascal-iter a b count n)
;;   (if (= count = 0)
;;       1
;;       (pascal-iter (+ a b) b (- count 1) n)
      ;; (pascal-iter a (+ a b) (- count 1) n)))
;; Function to generate the next row from the previous row
(define (next-row prev-row)
  (append (list 1)
          (for/list ([i (in-range (- (length prev-row) 1))])
            (+ (list-ref prev-row i) (list-ref prev-row (+ i 1))))
          (list 1)))

;; Iterative function to generate Pascal's triangle up to row n
(define (pascal-triangle n)
  (define (iter current-row current-level max-level)
    (if (= current-level max-level)
        (list current-row)
        (cons current-row
              (iter (next-row current-row) (+ current-level 1) max-level))))
  (iter '(1) 1 n))
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (even lst)
  (cond
    ((empty? lst) '())  ; If the list is empty, return an empty list
    ((= (remainder (first lst) 2) 0)  ; If the first element is even
     (cons (first lst) (even (rest lst))))  ; Include the first element and recurse on the rest of the list
    (else
     (even (rest lst)))))  ; If the first element is odd, just recurse on the rest of the list

(define (has-letter? word letter)
  (string-contains? (symbol->string word) letter))

(define (words-with lst letter)
  (cond
    ((empty? lst) '())  ; Base case: If the list is empty, return an empty list
    ((member letter (first lst))  ; If the first word contains "e"
     (cons (first lst) (words-with (rest lst) letter)))  ; Include it and recurse
    (else
     (words-with (rest lst) letter))))  ; Otherwise, skip it and recurse


;; (define (has-letter? word letter)
;;   (string-contains? (symbol->string word) letter))

;; (define (words-with lst letter)
;;   (cond
;;     [(empty? lst) '()]  ; Base case: If the list is empty, return an empty list
;;     [(has-letter? (first lst) letter)  ; If the first word contains the specified letter
;;      (cons (first lst) (words-with (rest lst) letter))]  ; Include it and recurse
;;     [else
;;      (words-with (rest lst) letter)]))
;; (define (plural wd)
;;   (word wd 's))
;; (define (plurl wd)
;;   (if (equal? (last wd) 'y)
;;       (word (bl wd) 'ies)
;;       (word wd 's)))
;;
; Define the keep function
(define (keep pred sent)
  (cond
    [(empty? sent) '()]  ; Base case: If the list is empty, return an empty list
    [(pred (first sent))  ; If the predicate is true for the first element
     (cons (first sent) (keep pred (rest sent)))]  ; Include the element and recurse
    [else
     (keep pred (rest sent))]))  ; Otherwise, skip the element and recurse
