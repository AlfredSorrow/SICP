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

(define (h n) (A 2 n)) ; = 2 power 2 n-1 times (for example: (A 2 4) => 2^2^2^2))

(define (k n) (* 5 n n)) ; = 5 * n^2

;;;Exercise 1.11


(define (f-recurs n)
  (cond ((< n 3) n)
        (else (+ (f-recurs (- n 1)) (f-recurs (- n 2)) (f-recurs (- n 3))))))

(define (f-iter n)
  (define (iter counter a b c)
    (cond ((< n 3) n)
          ((> counter n) (+ a b c))
          (else (iter (+ counter 1) b c (+ a b c)))))
  (iter 4 0 1 2))


(check-equal? (f-recurs 5) (f-iter 5))
(check-equal? (f-recurs 20) (f-iter 20))
(check-equal? (f-recurs 2) (f-iter 2))

;;;Exercise 1.12

(define (pascal row col)
  (cond ((or (> col row) (= col 0)) 0)
        ((or (= col row) (= col 1)) 1)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

(check-equal? (pascal 0 0) 0)
(check-equal? (pascal 5 3) 6)
(check-equal? (pascal 6 3) 10)
(check-equal? (pascal 2 1) 1)

;;;Exercise 1.16

(define (exp b n)
  (if (= n 0)
      1
      (* b (exp b (- n 1)))))

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (super-fast-exp b n)
  (define (iter b n product)
    (cond ((= n 0) product)
          ((even? n) (iter (square b) (/ n 2) product))
          (else (iter b (- n 1) (* product b)))))
  (iter b n 1))

(check-equal? (exp 0 0) (super-fast-exp 0 0))
(check-equal? (exp 5 8) (super-fast-exp 5 8))
(check-equal? (super-fast-exp 2 10) 1024)


;;;Exercise 1.17

(define (double a) (+ a a))
(define (halve a) (/ a 2))

(define (multiply a b)
  (cond ((< b 2) a)
        ((even? b) (multiply (double a) (halve b)))
        (else (+ a (multiply (double a) (halve (- b 1)))))))

(check-equal? (multiply 0 0) (* 0 0))
(check-equal? (multiply 5 8) (* 5 8))
(check-equal? (multiply 13 25) (* 13 25))

;;;Exercise 1.18

(define (multiply-iter a b)
  (define (iter a b product)
    (cond ((< b 2) (+ a product))
          ((even? b) (iter (double a) (halve b) product))
          (else (iter (double a) (halve (- b 1)) (+ a product)))))
  (iter a b 0))

(check-not-equal? (multiply 5 2) (multiply-iter 5 3))
(check-equal? (multiply 6 8) (multiply-iter 6 8))
(check-equal? (multiply 13 25) (multiply-iter 13 25))

;;;Exercise 1.19

(define (fib n)
  (define (fib-iter a b p q n)
    (cond ((= n 0) b)
          ((even? n) (fib-iter a
                               b
                               (+ (* p p) (* q q))
                               (+ (* 2 p q) (* q q))
                               (/ n 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- n 1)))))
  (fib-iter 1 0 0 1 n)
  )

(check-equal? (fib 0) 0)
(check-equal? (fib 6) 8)
(check-equal? (fib 15) 610)