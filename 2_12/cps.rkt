#lang racket

(require racket/trace)

(define !
  (λ (n)
    (cond
      [(zero? n) 1]
      [else (* n (! (sub1 n)))])))

(define !-cps
  (λ (n k)
    (cond
      [(zero? n) (k 1)]
      [else (!-cps (sub1 n)
                   (λ (v) (k (* n v))))])))

(define length
  (λ (l)
    (cond
      [(null? l) 0]
      [else (add1 (length (cdr l)))])))

(trace length)

(define length-cps
  (λ (l k)
    (cond
      [(null? l) (k 0)]
      [else (length-cps (cdr l)
                        (λ (v) (k (add1 v))))])))

(trace length-cps)

(define rember8
  (λ (l)
    (cond
      [(null? l) '()]
      [(pair? (car l)) (cons (rember8 (car l)) (rember8 (cdr l)))]
      [(eqv? (car l) 8) (rember8 (cdr l))]
      [else (cons (car l) (rember8 (cdr l)))])))

(trace rember8)

(define rember8-cps
  (λ (l k)
    (cond
      [(null? l) (k '())]
      [(pair? (car l)) (rember8-cps (car l)
                                (λ (a)
                                  (rember8-cps (cdr l)
                                           (λ (d)
                                             (k (cons a d))))))]
      [(eqv? (car l) 8) (rember8-cps (cdr l)
                                 (λ (v) (k v)))]
      [else (rember8-cps (cdr l)
                     (λ (d) (k (cons (car l) d))))])))

(trace rember8-cps)
