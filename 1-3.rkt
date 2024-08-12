#lang racket
(require racket/base)
(require threading)

(define (cube a) (* a a a))
(define (square a) (* a a))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;;;1.3.1;;;
(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
         (sum-cubes (+ a 1) b))))
(define (inc n) (+ n 1))

(define (sum-cubes-ho a b)
  (sum cube a inc b))

;; (define (pi-sum a b)
;;   (if (> a b)
;;       0
;;       (+ (/ 1.0 (* a (+ a 2)))
;;          (pi-sum (+ a 4) b))))

(define (identity x) x)

(define (sum-integers-ho a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;;;; Ex. 1.29 ;;;;;
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (sum-term k)
    (cond  ((= k 0) (y 0))
           ((= k n) (y n))
           ((even? k) (* 2 (y k)))
           (else (* 4 (y k)))))
    ;; (* (sum sum-term a inc n) (/ h 3.0)))
    (* (sum-iter sum-term a inc n) (/ h 3.0)))
;;;;;;Ex. 1.30 ;;;;;;
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cube-iter a b)
  (sum-iter cube a inc b))

;;;;; Ex. 1.31;;;;;;;
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (compose f g)
  (λ (x) (f (g x))))

(define (approx-pi n)
  (define (inc-2 x) (+ x 2))
  (* 4.0 (product (lambda (x) (/ (* x (+ x 2)) (* (+ x 1) (+ x 1))))
                  2 inc-2 (* n 2))
  ))


(define (wallis n k)
  (define (num-term k)
    (* (square n) 4))
  (define (denom-term k)
    (- (* (square n) 4) 1))
  (define numerator (product num-term n inc k))
  (define denominator (product denom-term n inc k))
  (* 2 (/ numerator denominator)))

(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
    (iter (next a) (* (term a) result))))
  (iter a 1))
;;;;;;;;;;Ex. 1.32;;;;;;;;;;;;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))
(define (mult a b) (* a b))

(define (factorial a b)
  (accumulate * 1 identity a inc b))

(define (summation a b)
  (accumulate + 0 identity a inc b))

;;;;;;;;;;Ex. 1.33;;;;;;;;;;;;;;;
(define (filter-accumulate combiner filterf null-value term a next b)
  ;; (displayln (format "a: ~a"a))
  (if (> a b)
      null-value
      (if (filterf a)
          (combiner (term a) (filter-accumulate combiner filterf null-value term (next a) next b))
     (filter-accumulate combiner filterf null-value term (next a) next b))))

(define (filter-acc-iter combiner filterf null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filterf a)
            (iter (next a) (combiner result (term a)))
        (iter (next a) (combiner result)))))
  (iter a null-value))

(define (sum-even a b)
  (filter-acc-iter + even? 0 identity a inc b))
;;;;;;;;1.33.a and .b;;;;;;;;;
(define (square-prime a b)
  (filter-accumulate + prime? 0 square a inc b))

(define (relative-prime? a b)
  (= (gcd a b) 1))


(define (euler-totient a b)
  (define (product-r-primes a)
    (relative-prime? a b))
  (filter-accumulate * product-r-primes 1 square a inc b))
;;;;;;;;

(define (gcd a b)
  ;; (displayln (format "Current a: ~a, b: ~a" a b))
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (smallest-div n) (find-div n 2))

(define (find-div n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-div n (next test-div)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-div n)))

(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))

(define (chunks-of lst size)
  (define (chunks-of-helper lst size)
    (if (null? lst)
        '()
        (let* ([chunk (take lst size)]
               [rest (drop lst size)])
          (cons chunk (chunks-of-helper rest size)))))
  (chunks-of-helper lst size))

(define (first-n-odds n)
  (~>> (range n)
       (map (λ (x) (+ 1 (* 2 x))))))

(define (product-of-list lst)
  (apply * lst))

(define (sum-list lst)
  (define (sum-helper lst acc)
    (if (null? lst)
        acc
        (sum-helper (rest lst) (+ (first lst) acc))))
  (sum-helper lst 0))

(~>> (first-n-odds 10000)
     (chunks-of _ 2)
     (map product-of-list)
     (map (λ (x) (/ 8.0 x)))
     (sum-list))
;;;;;;;;;;;;;;;1.3.2  Lambda;;;;;;;;
(define (pi-sum-lambda a b)
  (sum (λ (x) (/ 1.0 (* x (+ x 2))))
       a
       (λ (x) (+ x 4))
       b))

;;;;;;;;;;;;;;;
