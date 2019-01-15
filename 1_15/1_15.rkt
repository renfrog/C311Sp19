#lang racket

(define λ->lumbda
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       `,y]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       `(lumbda (,x) ,(λ->lumbda body))]
      [`(,rator ,rand)
       `(,(λ->lumbda rator) ,(λ->lumbda rand))])))

(define ω
  (λ (x) (x x)))

#;
(define Ω
  (ω ω))

(define free?
  (λ (e var)
    (match e
      [`,y
       #:when (symbol? y)
       (eqv? y var)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (if (eqv? var x)
           #f
           (free? body var))]
      [`(,rator ,rand)
       (or (free? rator var) (free? rand var))])))

(define bound?
  (λ (e var)
    (match e
      [`,y
       #:when (symbol? y)
       #f]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (or (and (eqv? x var) 
                (free? body var))
           (bound? body var))]
      [`(,rator ,rand)
       (or (bound? rator var) (bound? rand var))])))

