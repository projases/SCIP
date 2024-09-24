#lang racket
(require racket/base)

(define odds (list 1 3 5 7 9 11 13 15 17 19 21))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append-l list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append-l (cdr list1) list2))))
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

;;;;;;;;;;;Ex. 2.20;;;;;
(define integers (list 1 2 3 4 5 6 7 8 9))

(define (add-1 a . rest)
  (map (lambda (x) (+ x 1)) (cons a rest)))

;; (define (add-2 a . rest)
;;   (define (helper-fn lst)
;;     (if (null? lst)
;;         '()
;;         (cons (+ car lst) 2) (helper-fn (cdr lst))))
  ;; (helper-fn (cons a rest)))

(define (add-2 a . rest)
  (define (add-2-helper lst)
    (if (null? lst)
        '()  ;; Base case: return an empty list when the input list is empty
        (cons (+ (car lst) 2) (add-2-helper (cdr lst)))))  ;; Add 1 to the first element and recurse over the rest
  (add-2-helper (cons a rest)))  ;; Combine the first argument with the rest of the arguments and apply the helper function

(define (same-parity a . rest)
  (filter (λ (x)
            (if (odd? a)
                (odd? x)
                (even? x)))
          (cons a rest)))

(define (same-parity-gpt a . rest)
  (define (helper lst)
    (if (null? lst)
        '()  ;; Base case: return an empty list if there are no more elements
        (if (if (odd? a)
                (odd? (car lst))
                (even? (car lst)))  ;; Check if current element has the same parity as `a`
            (cons (car lst) (helper (cdr lst)))  ;; If it does, include it in the result
            (helper (cdr lst)))))  ;; Otherwise, skip it
  (helper (cons a rest)))  ;; Combine `a` with the rest of the arguments and process with `helper`

;; ;
;;;;;;;;;;
(define nil '())
(define (scale-list items factor)
  (if (null? items)
      1;; nil
      (cons (* (car items) factor)
            (scale-list (cdr items)
                        factor))))
(define (map-sicp proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list-map items factor)
  (map (λ (x)
         (* x factor))
       items))
;;;;;Ex. 2.21;;;;
(define (square-list item)
  (if (null? item)
      nil
      (cons (sqr (car item)) (square-list (cdr item)))))

(define (square-list-map items)
  (map (λ (x)
         (sqr x))
       items))
;;;;;;;;;Ex. 2.22;;;;;
(define (square-list-louis items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items nil))
;;
;; 1 2 3 ()
;; 2 3 cons 1^2 ()
;; 3   cons 2^2 1^2
;; ()  cons 3^2 2^2 1^23
;; change order of arguments iter
;; 1 2 3 ()
;; 2 3 cons () 1^2
;; 3 cons (() 1^2) 2^2
;; () cons ((() 1^2) 2^2) 3^2
;; ;;; Ex. 2.23;;;;;
(define (for-each proc lst)
  (cond
    ((null? lst) #t)
    (else
      (proc (car lst))
      (for-each proc (cdr lst)))))
;;;;; Ex. 2.24;;;
(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
      ((not (pair? x)) 1)
      (else (+ (count-leaves (car x))
         (count-leaves (cdr x))))))

(define (print-leaves x)
  (cond ((null? x) (void))
        ((not (pair? x)) (displayln x))
        (else (print-leaves (car x))
              (print-leaves (cdr x)))))
;;;Ex. 2.24
;;; box and pointer
;;; 1|-> 2(3 4)|-> 2|0
;;;             -> (3 4)|-> 3|-> 4|0
;;; tree
;; (list 1 (list 2 (list 3 4)))

;; (1)     (2 ( 3 4))

           ;; (3)  (4)
;;;;;;Ex. 2.25;;;;
(define lst1 (list 1 3 (list 5 7) 9))
(define lst2 (list (list 7)))
(define lst3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (find-leaf x lst)
  (cond ((null? lst) (void))
        ((and (not (pair? x)) (= x 7)) (displayln x))
        (else (find-leaf x (car lst))
              (find-leaf x (cdr lst)))))
;

;;;;;;Ex. 2.26;;;;;
;;(define x (list 1 2 3))
;;(define y (list 4 5 6))

;; (append x y) (1 2 3 4 5 6)
;; (cons x y) ((1 2 3) 4 5 6)
;; (list x y) ((1 2 3) (4 5 6))

;;; Ex. 2.27;;;;;
(define lst4 '(1 (2 3) 5 (6 7 8)))
(define (deep-reverse lst)
  (cond ((null? lst) nil)
        ((not (pair? lst)) lst)
        (else (append (deep-reverse (cdr lst))
              (list (deep-reverse (car lst)))))))
 (define (deep-reverse-jz x) ;;beautiful solution by jz
   (if (pair? x)
       (append (deep-reverse (cdr x))
               (list (deep-reverse (car x))))
       x))
 (define (deep-reverse-mod l) ;;modified lambdalef's version to work with non-lists
   (cond ((pair? l)
         (reverse (map deep-reverse-mod l)))
         (else l)))

;;;;Ex. 2.28;;;;;
(define (fringe lst)
    (cond ((null? lst) '())
          ((not (pair? lst)) (list lst))
          (else (append (fringe (car lst))
                (fringe (cdr lst))))))

(define (fringe-map lst)
  (if (pair? lst)
      (apply append(map fringe-map lst))
      (list lst)))

(define fringe-racket flatten)
;;;;Ex. 2.29;;;;;;;;;
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (car mob))
(define (right-branch mob)
  (cdr mob))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
      (cdr branch))

 (define (branch-weight branch)
   (cond ((not (pair? (right-branch branch)))
           (branch-structure branch))
         ((pair? (left-branch branch))
          (+ (branch-weight (left-branch branch))
             (branch-weight (right-branch branch))))
          ((not (pair? (left-branch branch)))
           (branch-weight (right-branch branch)))))

(define (total-len-br branch)
    (cond ((not (pair? (branch-structure branch)))
          (branch-length branch))
          (else (+ (branch-length branch)
                 (total-len-br (left-branch (branch-structure branch)))
             (total-len-br (right-branch (branch-structure branch)))))))

(define (balanced? mob)
  (= (* (total-len-br (left-branch mob))
        (branch-weight (left-branch mob)))
     (* (total-len-br (right-branch mob))
        (branch-weight (right-branch mob)))))

(define mob2
  (make-mobile
    (make-branch 6 (make-mobile (make-branch 4 3) (make-branch 3 2)))
    (make-branch 5 7)))

(define lb (left-branch mob2))
(define mob1 (make-mobile
              (make-branch 5 6)
              (make-branch 6 5)))

(define (total-weight mob)
  (+ (branch-weight (left-branch mob))
     (branch-weight (right-branch mob)) ))


;;;; Berkeley lectures homework;;;;
(let ((fact
       (λ (f)
         (λ (n)
           (if (= n 0)
               1
               (* n ((f f) (- n 1))))))))
  ((fact fact) 4))
 ;;;; Y combinator, recursion using lambda calculus
(((λ (f)
   ((λ (x) (x x))
    (λ (x)
      (f (λ (y) ((x x) y))))))
 (λ (f)      ;; applying factorial logic using lambdas
   (λ (n)
     (if (zero? n)
         1
         (* n (f (- n 1))))))) 4)
;;;;;;;Ex. 2.30; ;;;

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (sqr tree ))
        (else (cons (square-tree (car tree) )
                    (square-tree (cdr tree) )))))
(define (sq-tree tree)
  (cond ((null? tree) nil)
        ((list? tree)
         (map sq-tree tree))
        (else (sqr tree))))


(define t
  (list
    1
    (list 2 (list 3 4) 5)
    (list 6 7)
    10))
;;;Ex. 2.31;;;
(define (tree-map f tree)
  (if (pair? tree)
      (map (λ (x)
             (if (pair? x)
                 (tree-map f x)
                 (f x)))
           tree)
      (f tree)))

(define (tree-map-2 f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree ))
        (else (cons (tree-map-2 f (car tree) )
                    (tree-map-2 f (cdr tree) )))))
;;; Ex. 2.32;;;
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (x)
                            (cons (car s)
                                  x))
                      rest)))))
;; we go call until we reach '(), then we backtrack
;; append nil (cons '() 3) -> '(() 3)
;; append '(() 3) (cons 2 '()) (cons 2 3) -> ('() 3 2 (2 3))
;; append ...... -> '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;;
;;;
