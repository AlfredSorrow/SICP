#lang sicp

(#%require rackunit)

(define (cube x) (* x x x))

(check-equal? (+ (let ((x 3))
                   (+ x (* x 10))
                   ) 5) 38)


;;; Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

(check-equal? ((double inc) 5) 7)
(check-equal? ((double (double inc)) 5) 9)
(check-equal? (((double (double double)) inc) 5) 21)

;;; Exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))


(check-equal? ((compose (lambda (x) (* x x)) inc) 6) 49)

;;; Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (square x) (* x x))

(check-equal? ((repeated square 2) 5) 625)


