#lang sicp

(#%require rackunit)
;;;Exercise 1.1

(check-equal? 10 10)

(check-equal? (+ 5 3 4) 12)

(check-equal? (- 9 1) 8)

(check-equal? (/ 6 2) 3)

(check-equal? (+ (* 2 4) (- 4 6)) 6)

(define a 3)
(define b (+ a 1))


(check-equal? (+ a b (* a b)) 19)


(check-equal? (= a b) #f)

(check-eq? (if (and(> b a) (< b (* a b)))
               b
               a) 4)



(check-equal? (cond ((= a 4) 6)
                    ((= b 4) (+ 6 7 a))
                    (else 25)) 16)

(check-equal? (+ 2 (if (> b a) b a)) 6)

(check-equal? (* (cond ((> a b) a)
                       ((< a b) b)
                       (else -1))
                 (+ a 1)) 16)

;;;Exercise 1.2

(check-equal? (/  (+ 5 4
                     (- 2
                        (- 3
                           (+ 6
                              (/ 4 5)))))
                  (* 3
                     (- 6 2)
                     (- 2 7))) -37/150)


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

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? guess)
    (<
     (/
      (abs
       (- guess (improve guess)))
      guess)
     0.001))

  (sqrt-iter 1.0))

(check-equal? (round (sqrt 9)) 3.0)


;;; Execrcise 1.8
(define (qbrt x)
  (define (qbrt-iter guess)
    (if (good-enough? guess)
        guess
        (qbrt-iter (improve guess))))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))


  (define (good-enough? guess)
    (<
     (/
      (abs
       (- guess (improve guess)))
      guess)
     0.001))

  (define (square x) (* x x))
  (qbrt-iter 1.0))

(check-equal? (round (qbrt 27)) 3.0)
(check-equal? (round (qbrt 64)) 4.0)