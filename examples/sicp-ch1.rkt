#lang racket

;; # Chapter 1
;; Expressions and abstraction

;; ## Dave's notes
;; The author's believe computers are magically awesome.
;;
;; Goal of the book: starting with high school algebra and go from that to making a programming language. 
;; We start then with the idea of a machine.

;; ## 1.1 Elements of Programming

;; There are *primitives*, the most basic elements.

42
3.7

;; Then there are combinations.

(+ 42 37)
(- 1000 224)

;; etc
;; ### 1.1.1 Expressions

;; An expression could be a primitive or a combination of operators:

(* (+ 2 (* 4 6)) (+ 3 5 7))

;; ### 1.1.2 Naming and the Environment

(define size 2)

size

(+ size 10)

;; Scheme is very free with its symbols, you can include punctutation

(define really? 42)
(define <-> 13)
(+ <-> really?)

;; You can also name basic operations

(define op +)
(op 2 2)

;; Scheme uses *applicative order* which means it evaluates arguments into primitive values first, which is how most programming languages work.

;; 
;; Note that `define` is a *special form* in scheme. It behaves differently, you can define undefined things, it doesn't operate in according to the same rules as the rest of the procedures.

;; ### 1.1.6 Conditional Expressions and Predicates

(define (abs x)
  (if (< x 0) (- x) x))

;; or you can use `cond`. Both `if` and `cond` are special forms. There is also `and`, `or` and `not` 


;; ## Exercises
;;
;; ### Exercise 1.1
;; What is the result of the following expressions?

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)

(if (and (> b a) (< b (* a b)))
  b a)

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))

(+ 2 (if (> b a) b a))

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;; ### Exercise 1.2
;; Translate the following expression into prefix form:
;; $$ \frac{5 + 4 + (2 - (3 - (6 + \frac 4 5)))}{3(6-2)(2-7)} $$

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;; ### Exercise 1.3
;; Define a procedure `(sum-squares-two-largest a b c)` that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.

(define (sum-squares-two-largest a b c)
  (cond ((and (< a b) (< a c))
         (+ (* b b) (* c c)))
        ((and (< b a) (< b c))
         (+ (* a a) (* c c)))
        (else (+ (* b b) (* a a)))))

(= (sum-squares-two-largest 2 3 4) 25)
(= (sum-squares-two-largest 4 2 3) 25)
(= (sum-squares-two-largest 4 3 2) 25)
(= (sum-squares-two-largest 2 2 2) 8)
         

;; ### Exercise 1.4
;; Consider the following:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(a-plus-abs-b 2 10)
(a-plus-abs-b 2 -10)

;; We can work out how this works:

'(a-plus-abs-b 2 10)
'((if (> 10 0) + -) 2 10)
'((+ 2 10))
'(12)

;; And for the other one

'(a-plus-abs-b 2 -10)
'((if (> -10 0) + -) 2 -10)
'((- 2 -10))
'(12)

;; So it works either way, I think the thing that is meant to be a bit surprising there is that we are conditionaly selecting an operator?

;; ### Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether the interpreter he is faced with is using applicative-order evaluation or normal-order evaluation. He defines the following two procedures.

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

'(test 0 (p))

;; Note that if I run this in racket I get an infinite loop.

;; Let's work this out in both applicative order and normal order, in applicative order we have it try to expand `(p)` to `(p)` and it gets stuck in an infinite loop doing so.  In normal order we have:

'(test 0 (p))
'(test 0 (p))
'(if (= 0 0) 0 (p))
'(0)

;; And the result should be 0.
