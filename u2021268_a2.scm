#lang racket

; Maaz Saeed - 2021268 - BCE
; SICP - Assignment # 02

;                                Question # 01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; METALINGUISTIC ABSTRACTION

; In the video section where the three techniques for controlling complexity
; of larger program is explained with Black-box abstraction and conventional interfaces of the three;
; the third one is making a new language or “Metalinguistic Abstraction”
; that allows the programmer to abstract/suppress irrelevant details and
; simplify the overall design process by building Domain-specific languages
; designed to solve a particular type of problem e.g., verilog/vhdl, SQL, etc.,
; meta-circular evaluator that is writing in the same language and 
; interprets the same language e.g., a lisp interpreter in lisp.
; These and others can help in extending the features of the language;
; and alleviate the cognitive burden of unimportant details in the design 
; process of larger systems, meanwhile also augmenting the process of debugging.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;                                Question # 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1.3

;sum the square of the numbers
(define sum-sqr (lambda (x y) (+ (sqr x) (sqr y)))) 

;returns the square of the number
(define sqr (lambda (x) (* x x)))

;less than or equal to
(define lte (lambda (x y) (<= x y))) 

;the main function that returns the sum of the square of the largest two numbers
(define max (lambda (a b c) 
                (cond ((and (lte a c) (lte a b)) (sum-sqr b c))
                      ((and (lte b c) (lte b a)) (sum-sqr a c))
                      (else (sum-sqr a b)))))

; Test Cases
(= (sum-sqr 2 3) (max 1 2 3))
(= (sum-sqr -1 -2) (max -1 -2 -3))
(= (sum-sqr 4 5) (max 3 4 5))
(= (sum-sqr 100 1000) (max 10 100 1000))
(= (sum-sqr 10 10) (max 10 10 10))
(= (sum-sqr -5 -4) (max -5 -5 -4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; The program evaluates to expressions depending on the predicate
; if b > 0 then (a + b)
; otherwise (a - b) => (a + b) (b < 0)
; hence the procedure boils down to a + |b|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

; (test 0 (p))

; Applicative-order evaluation will result in infinite recursion, since in applicative
; evaluaes the arguments and then applies the procedure, first evaluating 0 which yields 0
; and then (p) and since 'p'
; is a recursive function without a terminating case, the argument (p) will keep calling
; (p) infinitely. e.g., (test 0 (p))
;                                |
;                                v
;                               (p)
;                                |
;                                v
;                               (p)
;                                .
;                                .
;                                .

; Normal-order evaluation evaluates to 0, as Normal-order will fully expand the entire procedure
; before evaluating; hence on running the program, the program will
; reduce to something like '(if (= 0 0) 0 (p))' and since the predicate evaluates to true, 0 will be returned
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1.6

; Since Lisp uses applicative-order evaluation the procedure new-if will NOT be treated as
; a special form and hence using new-if proecdure in the sqr-iter program will result in an infinite loop that
; is the else clause which in this case is a procedure argument (sqrt-iter (improve guess x) x) will be called
; again with same arguments which will then call itself again with same arguments and so on resulting in an infinite
; loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 1.8

(define cube (lambda (x) (* x x x)))

(define err 0.0001)

; (define sqr (lambda (x) (* x x))) ; function already defined in Ex 1.3

(define improve (lambda (y x) (/ (+ (* 2 y) (/ x (sqr y))) 3)))

(define abs (lambda (x) (if (< x 0) (* -1 x)
                             x)))

(define good-enough? (lambda (guess x) (<= (abs (- (cube x) (cube guess))) err)))
                           

(define try (lambda (guess x)
              (if (good-enough? (improve guess x) guess)
               guess
               (try (improve guess x) x))))


(define cube-root (lambda (x) (try 1.0 x)))

; Test Cases
; taking cube of the cube root yields in a value with max margin of 0.0001
(cube-root 9)
(<= (- 9 (cube (cube-root 9))) err)
(<= (- 8 (cube (cube-root 8))) err)
(<= (- 27 (cube (cube-root 27))) err)
(<= (- 125 (cube (cube-root 125))) err)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



