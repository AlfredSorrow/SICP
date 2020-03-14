#lang sicp

(#%require rackunit)

;;;Exercise 1.9

#| 

In case:

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

It's recursive process:

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
...
(inc 8)
9


In case 

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

It's iterative process:

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

|#

;;;Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(check-equal? (A 1 10) 1024)
(check-equal? (A 2 4) 65536)
(check-not-equal? (A 3 3) 1024)

(define (f n) (A 0 n)) ; = 2n

(define (g n) (A 1 n)) ; = 2^n

(define (h n) (A 2 n)) ; = 2 power 2 n times (for example: (A 2 4) => 2^2^2^2)

(define (k n) (* 5 n n)) ; 5 * n^2

