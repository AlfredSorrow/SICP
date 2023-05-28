#lang sicp

(#%require rackunit)


(define (make-rat numer denom)
  (let ((g (gcd numer denom)))
    (cons (/ numer g) (/ denom g))))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define one-half (make-rat 1 2))

; (print-rat one-half) ; 1/2

(define one-third (make-rat 1 3))

; (print-rat (add-rat one-half one-third)) ; 5/6

; (print-rat (add-rat one-third one-third)) ; 6/9 -> 2/3


;;; Exercise 2.1

(define (make-rat2 numer denom)
  (let ((g (gcd numer denom)))
    (if (or (< denom 0) (< numer 0))
        (cons (/ (- (abs numer)) g) (/ (abs denom) g))
        (cons (/ numer g) (/ denom g)))))

; (print-rat (make-rat2 -2 -4))


;;; Exercise 2.2

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((x1 (x-point (start-segment s)))
        (x2 (x-point (end-segment s)))
        (y1 (y-point (start-segment s)))
        (y2 (y-point (end-segment s))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

; (print-point (midpoint-segment (make-segment (make-point 0 0) (make-point 4 4)))) ; (2,2)
; (print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 9 10)))) ; (5,6)
; (print-point (midpoint-segment (make-segment (make-point -10 -20) (make-point 5 15)))) ; (-2.5,-2.5)

;;; Exercise 2.3

(define (make-rectangle p1 p2)
  (cons p1 p2))

(define left-top-point car)
(define right-bottom-point cdr)

(define (width-rectangle r)
  (- (x-point (right-bottom-point r)) (x-point (left-top-point r))))

(define (height-rectangle r)
  (- (y-point (right-bottom-point r)) (y-point (left-top-point r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r) (height-rectangle r))))

(define (print-rectangle r)
  (newline)
  (display "left-top: ")
  (print-point (left-top-point r))
  (display "right-bottom: ")
  (print-point (right-bottom-point r))
  (display "width: ")
  (display (width-rectangle r))
  (newline)
  (display "height: ")
  (display (height-rectangle r))
  (newline)
  (display "area: ")
  (display (area-rectangle r))
  (newline)
  (display "perimeter: ")
  (display (perimeter-rectangle r))
  (newline))

;(print-rectangle (make-rectangle (make-point 0 0) (make-point 4 4))) ; (0,0) (4,4) 4 4 16 16


;;; Exercise 2.4

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p)))

(define (cdr2 z)
  (z (lambda (p q) q)))

(define (print-pair z)
  (newline)
  (display "(")
  (display (car2 z))
  (display ",")
  (display (cdr2 z))
  (display ")")
  (newline))

;(print-pair (cons2 1 3)) ; (1,3)

;;; Exercise 2.5

(define (cons3 x y)
  (* (expt 2 x) (expt 3 y)))

(define (car3 z)
  (if (= (remainder z 2) 0)
      (+ 1 (car3 (/ z 2)))
      0))

(define (cdr3 z)
  (if (= (remainder z 3) 0)
      (+ 1 (cdr3 (/ z 3)))
      0))

(define (print-pair2 z)
  (newline)
  (display "(")
  (display (car3 z))
  (display ",")
  (display (cdr3 z))
  (display ")")
  (newline))

; (print-pair2 (cons3 10 20)) ; (10, 20)


;;; Exercise 2.17

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(check-equal? (last-pair (list 23 72 149 34)) 34)
(check-equal? (last-pair (list 23)) 23)
(check-equal? (last-pair (list nil)) nil)


;;; Exercise 2.18

(define (reverse items)
  (define (iter inner-items result)
    (if (null? inner-items)
        result
        (iter (cdr inner-items) (cons (car inner-items) result))))
  (iter items nil))

(check-equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))

;;; Exercise 2.20

(define (same-parity . items)
  (define (iter inner-items result)
    (cond ((null? inner-items) result)
          ((= (remainder (car inner-items) 2) (remainder (car items) 2))
           (iter (cdr inner-items) (cons (car inner-items) result)))
          (else (iter (cdr inner-items) result))))
  (iter (reverse items) nil))

(check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))

