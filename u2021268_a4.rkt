#lang scheme

; Maaz Saeed, 2021268, Assignment-04

; Question 01

#| -- Lecture 2B: Compound Data --
 
 Abstraction via Compound Data, which is gluing together multiple pieces of data
 to form a whole. The simple idea of glue which joins together two pieces of data
 can be used to construct all sort of complicated data structures like trees, sets,
 hash tables, stack, queue etc., then abstracting i.e hiding the details of implementation
 and building an interface around it via selectors and constructors for use.
 This data along with methods alleviates the programmer of the implementation details.
 For example cons, car and cdr can be implemented in various ways either by closures
 and dispatching e.g.,
 (define (cons x y)
           (define (dispatch m)
             (cond ((= m 0) x)
                   ((= m 1) y)
             )
           )
           dispatch)
 (define (car z) (z 0))
 (define (cdr z) (z 1))
 The above three defintions is a customary way of defining the three functions
 however to the user, it is not of any concern to know the implementation to use.
|#

#| -- Lecture 3A: Henderson Escher --
 
 Metalinguistic abstraction, the idea of embedding a domain specific language
 in the language meanwhile retaining the access to the power of the language
 in which it is implemented. The picture language used as a tools for understanding
 how metalinguistic is a powerful abstraction method, by implementing three
 different layers of language built on top of one another, the lower level
 responsible for primitives like pictures, consisting of vectors, drawling
 lines etc., and then on top of it (middle layer), the language responsible
 for geometric positions the functions used for manipulating pictures, e.g.
, beside, rotate, flip, above etc., and finally the higher level layer on top of
 the middle layer which uses the language of lisp itself e.g., the combinations
 used to manipulate pictures as a whole. This allows the design of software to be
 seen at various levels, hence a change in one part would depend on the level of the
 language it resides at and since there is a language already built around that subclass,
 it is easier to make changes and keep continuty of the program intact.
 For example, if vectors are to be changed or modified, the
 level of language will be the lowest level, since that is where the primitives are
 implemented. This is a very robust desing system allowing for modification and ensuring
 continuity as compared to decomposition (tree like development),
 in which submodules are sensitive to changes.
|#


; Question 02

; Exercise 2.2
; constructors
(define (make-segment start end)
  (cons start end))

(define (make-point x y)
  (λ (n)
    (if (= n 0)
        x
        y)))
  
(define (x-point seg)
  (seg 0))

(define (y-point seg)
  (seg 1))

; selectors
(define (start-segment line)
  (make-point ((car line) 0) ((car line) 1)))

(define (end-segment line)
  (make-point ((cdr line) 0) ((cdr line) 1)))

; utility methods
(define (mid-point line)
  (let ((x1 (x-point (start-segment line)))
        (x2 (x-point (end-segment line)))
        (y1 (y-point (start-segment line)))
        (y2 (y-point (end-segment line))))
    (make-point (/ (+ x1 x2) 2.0) (/ (+ y1 y2) 2.0))))

(define (dist line)
  (let ((x1 (x-point (start-segment line)))
        (x2 (x-point (end-segment line)))
        (y1 (y-point (start-segment line)))
        (y2 (y-point (end-segment line))))
    (sqrt (+ (sqr (- y2 y1)) (sqr (- x2 x1))))))


(define (area line1 line2)
  (* (dist line1) (dist line2)))

(define (perimeter line1 line2)
  (+ (* 2 (dist line1)) (* 2 (dist line2))))

(define (print-point p)
  (display (format "(~a, ~a)\n" (x-point p) (y-point p))))

; end of class


; simple rectangle with width = 8 and height = 4
;    y-axis
;    ^
;    |
;   4|       o
;    |       |
;   2|       |
;    |       |
; ---o-------o-------> x-axis
;    | 2 4 6 8 10 12
;    |
(define p1 (make-point 0 0))    
(define p2 (make-point 8 0))
(define p3 (make-point 8 4))
(define line1 (make-segment p1 p2))
(define line2 (make-segment p2 p3))
(define width (dist line1))
(define height (dist line2))

(area line1 line2)
(perimeter line1 line2)

; Exercise 2.3
; make-point already defined in 2.2
(define (make-vect p1 p2)
  (let ((dx (- (y-point p2) (y-point p1)))
        (dy (- (x-point p2) (x-point p1))))
    (make-point dx dy)))

(define (my-dot-product v1 v2)
  (let ((x1 (x-point v1))
        (x2 (x-point v2))
        (y1 (y-point v1))
        (y2 (y-point v2)))
     (begin (display x1)
            (display y1)
            (display x2)
            (display y2)
            (newline)
            (+ (* x1 x2) (* y1 y2)))))

(define (orthogonal? v1 v2)
  (= (my-dot-product v1 v2) 0.0))

(define (mag v)
  (let ((x (x-point v))
        (y (y-point v)))
    (sqrt (+ (* y y) (* x x)))))

(define (calc-area v1 v2)
  (if (orthogonal? v1 v2)
      (* (mag v1) (mag v2))
      (error "The vectors have to be orthogonal in a rectangle!")))

(define (calc-peri v1 v2)
  (if (orthogonal? v1 v2)
      (+ (* 2 (mag v1)) (* 2 (mag v2)))
      (error "The vectors have to be orthogonal!")))


(define pi (make-point 0 0))    
(define pj (make-point 8 0))
(define pk (make-point 0 4))
(define r (calc-area (make-vect pi pj) (make-vect pi pk)))
(define p (calc-peri (make-vect pi pj) (make-vect pi pk)))
(display (format "area: ~a, perimeter: ~a" r p))
(newline)


; Exercise 2.4
(define (my-cons x y)
  (λ (m) (m x y)))
(define (my-car z)
  (z (λ (p q) p)))

(define (my-cdr z)
  (z (λ (p q) q)))

; Exercise 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

; Exercise 2.18
(define nil '()) ; no built-in nil in racket.
(define (my-reverse lst)
  (if (null? lst)
      nil
      (append (my-reverse (cdr lst))
              (cons (car lst) nil))))

; Exercise 2.21
(define (my-square-list items)
  (if (null? items)
      nil
      (cons (sqr (car items))
            (my-square-list (cdr items)))))


(define (square-list items)
  (map sqr items))

; Exercise 2.27
(define (deep-reverse lst)
  (define (build lst res)
    (cond ((null? lst) res)
          ((pair? (car lst)) (build (cdr lst) (append res (list (deep-reverse (car lst))))))
          (else (build (cdr lst) (append res (list (car lst)))))))
  (my-reverse (build lst (list))))

; Exercise 2.31
(define (sqr-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (sqr tree))
        (else (cons (sqr-tree (car tree))
                    (sqr-tree (cdr tree))))))

(define (tree-map fn tree)
  (map sqr-tree tree))
(define (square-tree tree) (tree-map (λ (x) (* x x)) tree))

; Exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ (x) (cons (car s) x)) rest)))))

; Exercise 2.33
(define (accumulate op initial sequence) 
   (if (null? sequence) 
       initial 
       (op (car sequence) 
           (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (λ (x y) (cons (p x) y)) nil sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (λ (_ y) (+ y 1)) 0 sequence))

; Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (λ (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (x) (matrix-*-vector cols x)) m)))

(define T '((1 2 3) (4 5 6)))
(define V '(1 2 3))
(define TT '((1 2) (1 2)))
(matrix-*-vector T V)

; Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; racket has foldr instead of fold-right.
(foldr / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(foldr list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))

; Exercise 2.40
; Helper functions
(define (prime? x) 
  (define (test divisor) 
    (cond ((> (* divisor divisor) x) true) 
          ((= 0 (remainder x divisor)) false) 
          (else (test (+ divisor 1))))) 
  (test 2)) 
  
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatten lst)
  (accumulate append '() lst))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; Answer
(define (unique-pairs n)
  (let ((list-n (enumerate-interval 1 n)))
    (flatten (map (λ (i) (map (λ (j) (list i j)) (enumerate-interval 1 (- i 1)))) list-n))))

; simplified prime-sum using unique-pairs
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Exercise 2.41
(define (unique-triplet n)
  (let ((list-n (enumerate-interval 1 n)))
    (flatten
     (flatten
      (map (λ (i)
             (map (λ (j) (map (λ (k) (list i j k))
                              (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1)))) list-n)))))

; returns all the triplets that equal to the given number s, if there are any triplets that is.
(define (ordered-triplet n s)
  (filter
   (λ (triplet) (= (+ (car triplet) (cadr triplet) (caddr triplet)) s))
   (unique-triplet n)))