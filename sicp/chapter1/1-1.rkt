#lang sicp

(#%require rackunit)
;;;Exercise 1.1

10
;10

(+ 5 3 4)
;12

(- 9 1)
;8

(/ 6 2)
;3

(+ (* 2 4) (- 4 6))
;6

(define a 3)
(define b (+ a 1))
(+ a b (* a b))
;19

(= a b)
;#f

(if (and (> b a) (< b (* a b)))
    b
    a)
;4

(cond ((= a 4) 6)
    ((= b 4) (+ 6 7 a))
    (else 25))
;16

(+ 2 (if (> b a) b a))
;6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1))
;16

;;;Exercise 1.2

(/  (+ 5 4 
        (- 2 
            (- 3 
                (+ 6 
                    (/ 4 5))))) 
    (* 3 
        (- 6 2)
        (- 2 7)))
;-37/150

;;;Exercise 1.3

(define (>= x y)
    (not (< x y)))

(define (square x) (* x x))

(define (sum-of-squares x y)
    (+ (square x) (square y)))

(define (sum-of-squares-of-two-highest a b c)
    (cond ((and (>= a b) (>= c b)) (sum-of-squares a c))
        ((and (>= a c) (>= b c)) (sum-of-squares a b))
        ((and (>= b a) (>= c a)) (sum-of-squares b c))))

(check-equal? (sum-of-squares-of-two-highest 3 4 5) 41)
(check-equal? (sum-of-squares-of-two-highest 5 2 1) 29)
(check-equal? (sum-of-squares-of-two-highest -3 -4 -5) 25)


;;;Exercise 1.3