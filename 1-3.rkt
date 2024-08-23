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

(define radius 4)
;; (let ((radius 5)
;;       (area (* pi (* radius radius))))
;;   (list radius area))
;;;;;Ex. 1.34;;;;;;
(define (f g) (g 2))
;; (f f) takes f as an argument which then gives f 2,
;; and tries to apply 2 as a function which is not procedure.
;;;;;;;;;;;;;;;
(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? x y)
  (< (abs (- x y )) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;; (half-interval-method (λ (x) (- (* x x x) (* 2 x) 3))
;;                       1.0
;;                       2.0)
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (displayln (format "guess: ~a" guess))
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; (fixed-point cos 1.0)

(define (sqrt x)
  (fixed-point (λ (y) (average y (/ x y)))
               1.0))
;;;;;Ex. 1.35 golden ratio;;;;;

(define (golden-ratio)
  (fixed-point (λ (y) (+ 1 (/ 1 y)))
               1.0))
;;;;;Ex. 1.36;;;;;;;;;
(define (x-to-the-x y)
  (fixed-point (λ (x) (/ (log y) (log x)))
               2.0))
;;;;;;;;;Ex. 1.37;;;;;
(define (cont-frac n d k)
  (if (= k 0)
      0
      (/ (n k) (+ (d k) (cont-frac n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter k result)
    (displayln (format "N: ~a D: ~a result: ~a" (n k) (d k) result))
    (if (= k 1)
      result
      (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

(define (cont-frac-iterck n d k) ;;; connor hoekstra solution
  (define (cf-iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (d i) (cf-iter (+ i 1)))))
  (cf-iter 1))

;;;;;;Ex. 1.38;;;;;

(define (euler-e k)
  (let ((N (λ (x) 1.0))
        (D (λ (x)
             (if (even? x)
                 (* 2 (quotient x 2))
                 1))))
  (+ 2 (cont-frac-iter N D k))))
;;;;;;Ex. 1.39 ;;;;;;;;; doesn't work (not even the solutions from wiki)
(define (tan-cf x k)
  (let ((N (λ (i) (if (= i 1) x (-(* x x )))))
        (D (λ (i) i) ))
    (cont-frac-iterck N D k)))

;;;;;;
(define (tan-cfa x k) ;; wiki solution
   (let ((a  (* x x)))
     (cont-frac-iterck (lambda (i) (if (= i 1) x a))
              (lambda (i) (+ (* i 2) 1))
              k)))

 (define (tan-cfw x k) ;;;; wiki solution
   (define (tan-cf-rec i)
     (let ((di (+ i (- i 1)))
           (x^2 (* x x)))
     (if (= i k)
         di
         (- di (/ x^2 (tan-cf-rec (+ i 1)))))))
   (/ x (tan-cf-rec 1)))

;;;;;;1.3.4 Procedures as returned values;;;;;
(define (average-damp f)
  (λ (x) (average x (f x))))

(define (sqrt-proc x)
  (fixed-point (average-damp (λ (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (λ (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (λ (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (λ (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newtons-sqrt x)
  (newtons-method
  (λ (y) (- (square y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (general-sqrt x)
  (fixed-point-of-transform
   (λ (y) (/ x y)) average-damp 1.0))

(define (general-sqrt-2 x)
  (fixed-point-of-transform
   (λ (y) (- (square y) x)) newton-transform 1.0))
;;;;; Ex. 1.40;;;;;
(define (cubic a b c)
  (λ (x) (+ (* x x x)
            (* a (* x x))
            (* b x)
            c)))
(define (cubic-roots a b c)
  (newtons-method (cubic a b c) 1.0))

;;;;;;;;Ex. 1.41;;;;;
(define (double f)
  (λ (x) (f(f x))))

; (((double (double double)) inc) 5)
; (2x)
; 2(2 x) = (4x)
; 2(4x 2x) = 16x
; 16 inc 5 = 21;

;;;;;Ex. 1.42;;;;
(define (compose f g)
  (λ (x) (f (g x))))

;;;;;Ex. 1.43;;;;;
(define (repeated f n)
    (if (= n 0 )
        (λ (x) x)
        (compose f (repeated f (- n 1)))))

(define (repeat f n)
  (λ (x)
    (define (iter counter acc)
      (if (= counter 0)
          acc
          (iter (- counter 1) (f acc))))
  (iter n x)))
;;;;;;;Ex. 1.44;;;;;;
(define (smooth f)
  (λ (x) (/ (+ (f (+ x dx))
               (f x)
               (f (- x dx)))
            3)))
 (define (n-fold-smooth f n)
   ((repeated smooth n) f))
;;;;; Ex. 1.45;;;;;;;;
(define (general-cbrt x)
  (fixed-point-of-transform
   (λ (y) (/ x (square y))) average-damp 1.0))

(define (general-root degree n)
  (fixed-point-of-transform
   (λ (y) (/ n (expt y (- degree 1))))
   (repeat average-damp degree) 1.0))
;;;;;
(define (iterative-improve enough? improve-guess)
  (λ (first-guess)
    (define (iter guess)
      (displayln (format "guess: ~a" guess))
      (if (enough? guess)
        guess
        (iter (improve-guess guess))))
    (iter first-guess)))


(define (square-root x)
  ((iterative-improve
    (λ (guess)
      (< (abs (- (square guess) x))
         0.001))
    (λ (guess)
      (average guess (/ x guess))))
   1.0))

(define (fixed-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))

(define (new-fixed-point f first-guess)
  ((iterative-improve
    (λ (x) (fixed-enough? x (f x)))
    f)
   first-guess))

(define (new-sqrt x)
  (new-fixed-point (λ (y) (average y (/ x y))) 1.0))

;; (define (new-fixed-point f)
;;   ((iterative-improve (λ (guess) (< (abs (- (f guess) guess)) tolerance))
;;                       f) 1.0))

