#lang racket

;; # SICP Chapter 1
;; *Building Abstractions with Procedures*

;; ## 1.1 Elements of Programming

;; There are *primitives*, the most basic elements.

42
3.7

;; And there are combinations of primitives:

(+ 42 37)
(- 1000 224)

;; ### 1.1.1 Expressions

;; An expression can be a primitive or a combination of operators:

(* (+ 2 (* 4 6)) (+ 3 5 7))

;; ### 1.1.2 Naming and the Environment

(define size 2)

size

(+ size 10)

;; Scheme is liberal with identifier names — punctuation is fine:

(define really? 42)
(define <-> 13)
(+ <-> really?)

;; Basic operations can also be named:

(define op +)
(op 2 2)

;; Scheme uses *applicative order* — arguments are evaluated to
;; primitive values before being passed in, like most languages.
;;
;; `define` is a *special form* — it doesn't follow the same
;; evaluation rules as ordinary procedures (e.g., its first argument
;; is a name, not an expression to evaluate).

;; ### 1.1.6 Conditional Expressions and Predicates

(define (abs x)
  (if (< x 0) (- x) x))

;; `if` is one of the special forms. `cond`, `and`, `or`, and `not`
;; are also useful for conditional logic.

;; ## Exercises

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
    b
    a)

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
;;
;; $$ \frac{5 + 4 + (2 - (3 - (6 + \frac{4}{5})))}{3(6-2)(2-7)} $$

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; ### Exercise 1.3
;; Define a procedure `(sum-squares-two-largest a b c)` that takes
;; three numbers and returns the sum of the squares of the two
;; larger ones.

(define (sum-squares-two-largest a b c)
  (cond ((and (< a b) (< a c)) (+ (* b b) (* c c)))
        ((and (< b a) (< b c)) (+ (* a a) (* c c)))
        (else                  (+ (* a a) (* b b)))))

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

;; The `if` returns an operator (`+` or `-`), and the result of the
;; `if` is then applied as the procedure of the outer combination.
;; Selecting an operator conditionally and applying it in one step.

;; ### Exercise 1.5
;; Ben Bitdiddle's test for applicative vs normal order evaluation:

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

;; `(test 0 (p))` under **applicative order** loops forever — Scheme
;; tries to evaluate `(p)` before passing it to `test`, and `(p)`
;; expands to `(p)` indefinitely. Racket uses applicative order, so
;; running this expression hangs.
;;
;; Under **normal order**, arguments are substituted unevaluated.
;; The expression reduces:
;;
;; - `(test 0 (p))`
;; - `(if (= 0 0) 0 (p))`
;; - `0`
;;
;; The unused `(p)` is never forced, and the result is `0`.
