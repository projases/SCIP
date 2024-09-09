#lang racket
(require racket/base)
(require threading)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;;;;Ex. 2.1;;;;;;;;;;
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
       (cons (/ (- n) g) (/ (- d) g))
    (cons (/ n g) (/ d g)))))


(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))
(define one-fourth (make-rat -1 4))

;;; Ex. 2.2 ;;;;;;;;;
(define (make-point x y)
  (cons x y))
(define (x-point x) (car x))
(define (y-point y) (cdr y))
(define (start-point x) (car x))
(define (end-point x) (cdr x))
(define origin (make-point 0 0))
(define point (make-point 4 4))

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))
(define (mid-point segment)
  (make-point
   (/ (+
       (x-point (start-point segment))
       (x-point (end-point segment)))
      2)
   (/ (+
       (y-point (start-point segment))
       (y-point (end-point segment)))
      2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
;;;;;;Ex. 2.3;;;;;;;;;

(define (segment-length segment)
 (sqrt (+ (sqr (- (x-point (end-point segment))
                     (x-point (start-point segment))))
          (sqr (- (y-point (end-point segment))
                     (y-point (start-point segment)))))))


(define (rectangle b h)
  (let ((origin (make-point 0 0))
        (b2 (make-point b 0))
        (h1 (make-point b h))
        (h2 (make-point 0 h)))
  (cons (cons origin (cons h2 h1) ) (cons origin b2))))


(define (rectangle-vertices b h point)
  (let ((B1 point)
        (B2 (make-point (+ (x-point point) b)
                        (y-point point)))
        (H1 (make-point (x-point point)
                        (+ (y-point point) h)))
        (H2 (make-point (+ (x-point point) b)
                        (+ (y-point point) h))))
  (cons B1 B2 H2 H1)))

 (define (rectangle2 vertices)
   (let ((base1 (make-segment (car vertices)
                             (car (cdr vertices))))
         (base2 (make-segment (car (cdr (cdr vertices)))
                              (car (cdr (cdr (cdr vertices))))))
         (side1 (make-segment (car vertices)
                              (car (cdr (cdr (cdr vertices))))))
         (side2 (make-segment (car (cdr vertices))
                              (car (cdr (cdr vertices))))))
   (cons base1 base2 side1 side2)))

;;;;;;


;;;;Ex. 2.4;;;
;; (define (cdr z)
;;   (z (λ (p q) q)))
;; ;
;;;Ex. 2.5;;;
(define (const a b)
 (* (expt 2 a) (expt 3 b)))

(define (prime-fact num a)
  (define (iter num acc)
    (if (not (= (modulo num a) 0))
        acc
        (iter (/ num a) (+ acc 1))))
  (iter num 0))

(define (first z)
  (prime-fact z 2))
(define (last z)
  (prime-fact z 3))
;;;;Ex. 2.6;;;;
(define zero (λ (f) (λ (x) x)))

(define (add-1 n)
  (λ (f) (λ (x) (f ((n f) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interesting code from SICP wiki jsdalton
 (define (int->church n)
   (if (= n 0)
       zero
       (add-1 (int->church (- n 1)))))

(define (church-to-int cn)
   ((cn (lambda (n) (+ n 1))) 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define inc (λ (n) (+ n 1)))

(define (add m n)
  (λ (f) (λ (x) ((m f) ((n f) x)))))

(define (mult m n)
  (λ (f) (λ (x) ((m(n f)) x))))

(define one (add-1 zero))
(define two (add-1 one))
(define three (add-1 two))

(((mult two three) inc) 0)

;;;;;; Ex. 2.7 2.8 2.9 2.10;;;
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0) (>= (upper-bound y) 0));;; Ex. 2.10;;
      (error "Division by interval that includes zero")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (upper-bound interval) (max (car interval) (cdr interval))) 

(define (lower-bound interval) (min (car interval) (cdr interval))) 

 (define (display-interval i) 
   (newline) 
   (display "[") 
   (display (lower-bound i)) 
   (display ",") 
   (display (upper-bound i)) 
   (display "]")) 

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define (sub-i x y)
  (add-interval x
                (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define x (make-interval 1 3))
(define y (make-interval 2 4))

;; 2.11 ...the juice is not worth the squeeze
;; Both intervals are positive: The smallest product comes from multiplying the smallest bounds, and the largest from the largest bounds.
;; Both intervals are negative: The largest positive product comes from the most negative numbers.
;; One interval is positive, the other negative: The smallest value comes from the largest positive multiplied by the smallest negative.
;; One interval spans zero, the other is positive/negative: Some combinations yield zero, and both negative and positive values can be included in the result.
;; Both intervals span zero: The result could be negative, positive, or include zero, depending on the mix of bounds.

;;Ex. 2.12;;;;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (- (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent interval)
  (/ (width interval) (center interval) ))

(define (make-center-percent c percent)
  (let ((err (* c percent)))
  (make-interval (- c err) (+ c err))))

;;;Ex. 2.13;;;
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define A (make-interval 1 1.1))
(define B (make-interval 2 2.1))
