#lang racket

;   MAAZ SAEED, 2021268, BCE, ASSIGNMENT # 03

;             QUESTION # 01
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Denotational semantics: This approach to formalizing the meaning of programming languages
;describes the behaviour of programs using mathematical notations. These notations can be
;used to verify the correctness of programs and algorithms, primarily utilizing functions
;as first-class citizens. First-class citizens possess certain rights and privileges, such as
;being named and stored in variables, being passed as arguments to other procedures,
;being returned as values, and, last but not least, being incorporated into data structures.
;Allowing functions to be first-class citizens can provide powerful abstractions; for example,
;the sigma notation used in mathematics sums up a function's values in a given range.
;With functions as first-class citizens, the sigma function can take a procedure as an argument
;to calculate the value and, alongside it, take another helper function to calculate
;the next value, and finally calculate the sum.

;Iterative and recursive procedures generally solve the same problem but evolve
;into two different processes. The iterative procedure can capture the entire
;state of the function in its variables. Therefore, if the function is exited
;at any point of its execution, it will have everything required to continue again.
;However, in recursive procedures, there are deferred operations that need to be evaluated,
;and thus halting a function at a given point cannot provide all the information needed to continue.
;This key difference allows compilers to perform tail-call optimization, ensuring that the
;recursive process only takes up one stack frame
;and updates the variables that capture the state of the program.


;             QUESTION # 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Question # 01, Exercise 1.17
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;         |
;         | a * (b / 2) + a * (b / 2) ; when b is even 
; a * b = |
;         | a + a * (b - 1)           ; when b is odd
;         |

(define (odd? n) (= (modulo n 2) 1))
(define (even? n) (= (modulo n 2) 0))

(define (halve n) (/ n 2))
(define (double n) (+ n n))

(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        (else (cond ((even? b) (double (fast-mult a (halve b))))
                    (else (+ a (fast-mult a (- b 1))))))))

(display "Question # 01, Exercise 1.17\n")
(display "Test Cases for fast multiplication (recursive)\n")
(= (fast-mult 2 3) 6)
(= (fast-mult 8 9) 72)
(= (fast-mult 100 80) 8000)
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question # 02, Exercise 1.18
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-mult-iter a b)
  (define (mult-iter x y g f)
    (cond ((= y 0) f)
          ((odd? y) (mult-iter x (- y 1) g (+ x f)))
          (else (mult-iter (double x) (halve y) g f))))
  (mult-iter a b a 0))

(display "Question # 02, Exercise 1.18\n")
(display "Test Cases for fast multiplication (iterative)\n")
(= (fast-mult 2 3) 6)
(= (fast-mult 8 9) 72)
(= (fast-mult 100 80) 8000)
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question # 03, Exercise 1.25
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The number of digits and memory required for larger exponentials
; increases very rapidly, and performing mod operation on such numbers
; can be extremely slow, the fast-mod function breaks it down into smaller mods
; before calculating the final mod result.

(display "Question # 03, Exercise 1.25\n")
(display "Explanation in comments, refer to line 63")
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;Question # 04, Exercise 1.26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Since scheme is applicative order that means
; the expression (* (expmod base (/ exp 2) m)
;                   (expmod base (/ exp 2) m))
; both the calls to expmod would be fully evaluated before the results are multiplied
; since expmod is recursive that means time complexity of the two together would be
; (log 2^n) which according to the power rule of logarithms is equal to (n*log2), since log 2 is a constant
; the time complexity becomes O(n)
; however squaring is a constant time operation with O(1)
; and thus time complexity with square(expmod base (/ exp 2) m)) would be O(log n)

(display "Question # 04, Exercise 1.26\n")
(display "Explanation in comments, refer to line 84")
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question #05, Exercise 1.30
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(display "Question # 05, Exercise 1.30\n")
(display "Test Cases for Summation\n")
(= (sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) 15)
(= (sum (lambda (x) x) -1 (lambda (x) (+ x 1)) 5) 14)
(= (sum (lambda (x) x) -2 (lambda (x) (+ x 1)) 5) 12)
(= (sum (lambda (x) x) -3 (lambda (x) (+ x 1)) 5) 9)

(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question #06, Exercise 1.32
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(display "Question # 06, Exercise 1.32\n")
; Test Cases for summation
(display "Test Cases for Summation\n")

(= (accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) 15)
(= (accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 6) 21)
(= (accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) -1 (lambda (x) (+ x 1)) 1) 0)
(= (accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) -3 (lambda (x) (+ x 1)) -1) -6)
(= (accumulate (lambda (x y) (+ x y)) 0 (lambda (x) x) -5 (lambda (x) (+ x 1)) -1) -15)

(display "Test Cases for Product\n")
; Test Cases for product
(= (accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) 120)
(= (accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 3) 6)
(= (accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 4) 24)
(= (accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) -3 (lambda (x) (+ x 1)) -1) -6)
(= (accumulate (lambda (x y) (* x y)) 1 (lambda (x) x) -4 (lambda (x) (+ x 1)) -1) 24)


; iterative process
(define (accumulate-iter combiner null-value term a next b)
  (define (acc-iter a result)
    (if (> a b)
        result
        (acc-iter (next a) (combiner (term a) result))))
  (acc-iter a null-value))

; Test Cases for summation using iterative accumulation
(display "Test Cases for Summation using iterative accumulation\n")

(= (accumulate-iter (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) 15)
(= (accumulate-iter (lambda (x y) (+ x y)) 0 (lambda (x) x) 1 (lambda (x) (+ x 1)) 6) 21)
(= (accumulate-iter (lambda (x y) (+ x y)) 0 (lambda (x) x) -1 (lambda (x) (+ x 1)) 1) 0)
(= (accumulate-iter (lambda (x y) (+ x y)) 0 (lambda (x) x) -3 (lambda (x) (+ x 1)) -1) -6)
(= (accumulate-iter (lambda (x y) (+ x y)) 0 (lambda (x) x) -5 (lambda (x) (+ x 1)) -1) -15)

(display "Test Cases for Product using iterative accumulation\n")
; Test Cases for product
(= (accumulate-iter (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 5) 120)
(= (accumulate-iter (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 3) 6)
(= (accumulate-iter (lambda (x y) (* x y)) 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 4) 24)
(= (accumulate-iter (lambda (x y) (* x y)) 1 (lambda (x) x) -3 (lambda (x) (+ x 1)) -1) -6)
(= (accumulate-iter (lambda (x y) (* x y)) 1 (lambda (x) x) -4 (lambda (x) (+ x 1)) -1) 24)

(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question # 07, Exercise 1.37
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; recursive
(define (cont-frac n d k)
  (define (redex i)
    (if (> i k)
        0.0
        (/ (n i) (+ (d i) (redex (+ i 1))))))
  (redex 0))

; iterative
(define (cont-frac-iter n d k)
  (define (iter r k)
    (if (< k 0)
        r
        (iter (/ (n k) (+ (d k) r)) (- k 1))))
  (iter (/ (n k) (d (+ k 1))) (- k 1)))


; calculating 1 / phi
(define (n i) 1.0)
(define (d i) 1.0)

(display "Question # 07, Exercise 1.37\n")
(define (loop i e)
  (if (> i e)
      (format "Loop terminated after ~a iterations" (- i 1))
      (begin (display (format "k = ~a, 1/phi = " i))
             (display (cont-frac-iter n d i))
             (display "\n")
                    (loop (+ 1 i) e))))

; at-least 11 iterations are needed to correctly calculate 1 / phi to 4 decimal places.
(loop 1 12)
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;Question # 08, Exercise 1.41
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Explanation
;lets call our function double-proc as f.
;calling (f (f f)) will result in (f f) applied twice.
;will yield (f(f(f(f x))))
;that is the function double applied 16 times in total.
;in mathematical notation that would be 2^4 calls,
;it all boils down to (f (f (f (f inc)))) with each nested function doubling the resultant function obtained
;thus result is (inc applied 16 times)... (inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc(inc 5))))))))))))))))
;which will evaluate to 21 when the argument applied to it is 5.

(display "Question # 08, Exercise 1.41\n")
(define (double-proc proc)
  (lambda (x) (proc (proc x))))

(define (inc x) (+ x 1))
(((double-proc (double-proc double-proc)) inc) 5)
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;Question # 09, Exercise 1.42
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (compose f g)
  (lambda (x) (f (g x))))

(define (sqr x) (* x x))

(display "Question # 09, Exercise 1.42\n")
((compose sqr inc) 6)
(display "\n\n")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;Question # 10, Exercise 1.43
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (repeated proc n)
  (define (find-nth n res)
    (if (<= n 1)
        res
        (find-nth (- n 1) (compose res proc))))
  (find-nth n proc))

(display "Question # 10, Exercise 1.43\n")
; Test Cases for finding nth repeated application of a function
(display "Test Cases for finding nth repeated application of a function\n")
(= ((repeated (lambda (x) (* x x)) 2) 5) 625)
(= ((repeated (lambda (x) (+ x 1)) 3) 5) 8)
(= ((repeated (lambda (x) x) 3) 5) 5) ; identity function is the proc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;