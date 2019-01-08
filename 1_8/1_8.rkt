#lang racket

(define rember
  (λ (v l)
    (cond
      [(null? l) '()]
      [(eqv? (car l) v) (rember v (cdr l))]
      [else (cons (car l) (rember v (cdr l)))])))

(define sum
  (λ (l)
    (cond
      [(null? l) 0]
      [(number? (car l)) (+ (car l) (sum (cdr l)))]
      [else (sum (cdr l))])))

(define +
  (λ (m n)
    (cond
      [(zero? n) m]
      [else (add1 (+ m (sub1 n)))])))

;test case: (+ 15 3) => 18
;natural recursion: (+ 15 2) => 17
; (add1 (+ 15 2)) => 18

(define *
  (λ (m n)
    (cond
      [(zero? n) 0]
      [else (+ m (* m (sub1 n)))])))

;test case: (* 15 4) => 60
;natural recursion: (* 15 3) => 45
; (+ 15 (* 15 3)) => 60

(define length
  (λ (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))

;test case: (length '(11 2 3 (cat dog) (5 6))) => 5
;natural recursion: (length '(2 3 (cat dog) (5 6))) => 4
; (add1 4) => 5


