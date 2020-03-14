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


;;;Exercise 1.4

; if b > 0 returns a + b, else a - b

;;;Exercise 1.5

; In applicative-order evaluation there will be endless loop, else it returns 0

;;;Exercise 1.6

; There will be endless loop, because of trying to evaluate every arguments of new-if

;;;Exercise 1.7
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
    (< 
        (/ 
            (abs 
                (- guess (improve guess x)))
                guess)
        0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; (sqrt 9)
; idk how test this correctly for now


;;; Execrcise 1.8

(define (qbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (qbrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))


(define (good-enough? guess x)
    (< 
        (/ 
            (abs 
                (- guess (improve guess x)))
                guess)
        0.001))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (qbrt x)
  (qbrt-iter 1.0 x))

; (qbrt 27)
; idk how test this correctly either





