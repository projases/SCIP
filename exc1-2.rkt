#lang racket
(require racket/base)
(require threading)


;;;;;;;;;;Euclid's algorithm;;;;;;
(define (gcd a b)
  ;; (displayln (format "Current a: ~a, b: ~a" a b))
  (if (= b 0)
    a
    (gcd b (remainder a b))))
;;;;;;;;;;;;1.2.6;;;;;;;;;
(define (smallest-div n) (find-div n 2))

(define (find-div n test-div)
  (cond ((> (square test-div) n) n)
        ((divides? test-div n) test-div)
        (else (find-div n (next test-div)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-div n)))



;;;;;;;;;;;;fermat test;;;;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
        (remainder
         (square (expmod base (/ exp 2) m))
         m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
(try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;;;;;;;;;;;Ex. 1.25;;;;;;;;
;; (define (timed-prime-test n)
;;   (newline)
;;   (display n)
;;   (start-prime-test n (runtime)))

;; (define (start-prime-test n start-time)
;;   (if (prime? n)
;;       (report-prime (- (runtime) start-time))))

;; (define (report-prime elapsed-time)
;;   (display " *** ")
;;   (display elapsed-time))
;;;;; racket implementation;;;;;


(define (timed-prime-test n)
  (newline)
  (display n)
  (let ([start-time (current-inexact-milliseconds)])
    (start-prime-test n start-time)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 1)
      (report-prime (- (current-inexact-milliseconds) start-time))
      (void))) ; Use (void) to indicate that nothing is done in the else clause

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes start)
 (odd-range start 0))

(define (odd-range bottom counter)
  (if (< counter 3)
        (if (prime? bottom)
            (begin
            (timed-prime-test bottom)
            (odd-range (next-odd bottom) (+ counter 1)))
        (odd-range (next-odd bottom) counter))
  (void)))

(define (next-odd n)
  (if (odd? n) (+ n 2)
      (+ n 1)))

;;;;;;;;;;;Ex. 1.23 ;;;;;;;;;;;
(define (next test-divisor)
  (if (= test-divisor 2)
      3
      (+ test-divisor 2)))
;;;;;;;;Ex. 1.24;;;;;;
(define (expmod-alt base exp m)
(remainder (fast-expt base exp) m))
;; While Alyssa's approach is correct in terms of
;; computing the right mathematical result,
;; it is less efficient than using an optimized
;; modular exponentiation algorithm.
;; For very large exponents, the direct computation of
;; baseexpbaseexp followed by the modulus operation
;; can be impractical due to the size of the intermediate result.
;;For a fast prime tester or other applications
;;where efficiency is crucial, using a modular exponentiation technique
;;(such as the one typically used in expmod) is generally preferable.
;;It avoids the computational overhead of handling very large numbers
;;and ensures the process remains efficient even for large exponents.
;;
;;;;;;;;Ex. 1.25;;;;;;;;;;
  ;; (define (test-nums a n)
  ;;   (if (< a n)
  ;;       a
        ;; (test-nums (+ a 1) n)))

;; (define (check-mod a b)
;;   (expmod (test-nums a b) b b ))

;; `test-nums` returns a function that generates numbers from `a` to `n - 1`.
(define (test-nums a n)
  (let ([current a])
    (λ ()
      (if (< current n)
          (let ([value current])
            (set! current (+ current 1))
            value)
          #f))))

(define (check-congruence a n)
  (define (check-number k)
    (= (expmod a (sub1 n) n) a))

  (for/and ([i (generate-range a n)])
    (check-number i)))
(define (generate-range a n)
  (in-range a n))

;; Example usage of test-nums
;; (define gen (test-nums 1 10))
;; (for ([i (in-range 10)])
;; (define (fermat-test-2 n a)
;;   (= (expmod a a n) a))

;; Check if n is a Carmichael number
(define (is-carmichael? n)
  ;; A Carmichael number must be composite
  (define (composite? k)
    (and (> k 1) (not (prime? k))))
  (if (composite? n)
      (let ([range (generate-range 2 n)])  ;; Check numbers from 2 to n - 1
        (for/and ([a range])
          (if (= (gcd a n) 1)  ;; gcd(a, n) must be 1
              (check-congruence a n)
              #t)
          #f)  ;; If gcd(a, n) != 1, we skip this check
     
      )
  #f))  ;; If n is not composite, return #f
(define (fermat-test-2 n a)
  (= (expmod a n n) a))
;; Check carmichael numbers
(define (is-car? n)
  (define (fermat-all n)
  (~> (range 1 n)
      (andmap (λ (a) (fermat-test-2 n a)) _)))
  (and (fermat-all n) (not (prime? n))))

;;;;;;;;Ex. 1.28;;;;;;;;;;;;;;;;;;;;;

;; (define (miller-rabin-test n)
;;   (define (try-it a)
;;     (or (= (expmod a (- n 1) n) 1))
;;     (= (expmod a (- n 1) n) (- n 1)))
;; (try-it (+ 1 (random (- n 2)))))

(define (miller-rabin-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
        (else false)))
;; (define (miller-rabin-test n)
;;   (define (try-it a)
;;     (let ((result (expmod a (- n 1) n))) ; Compute a^(n-1) % n
;;       (or (= result 1)                 ; Check if the result is 1
;;           (= result (- n 1)))))        ; Check if the result is n-1
;;   (try-it (+ 1 (random (- n 1)))))
(define (miller-rabin-test n)
  (define a (+ 1 (random (- n 1)))) ; Random base a between 1 and n-1
  (or (= (remainder (expt a (- n 1)) n) 1)  ; Check if result is 1
      (= (remainder (expt a (- n 1)) n) (- n 1))))
;; (define (miller-rabin-test n)
;;   (define (try-it a)
;;     (define exp-result (expmod a (- n 1) n))  ; Compute a^(n-1) mod n
;;     (or (= exp-result 1)                   ; Check if the result is 1
;;         (= exp-result (- n 1))))           ; Check if the result is n-1
;;   (define a (+ 1 (random (- n 1))))          ; Random base a between 1 and n-1
;;   (try-it a))
(displayln (miller-rabin-prime? 561 5))
(displayln (miller-rabin-prime? 1105 5))
(displayln (miller-rabin-prime? 1729 5))
(displayln (miller-rabin-prime? 2465 5))
(displayln (miller-rabin-prime? 2821 5))
(displayln (miller-rabin-prime? 6601 5))
(displayln (miller-rabin-prime? 13 5))
;; (define (fast-prime? n times)
;;   (cond ((= times 0) true)
;;         ((fermat-test n) (fast-prime? n (- times 1)))
;;         (else false)))
;
;;;;;;;;
