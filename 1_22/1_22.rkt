#lang racket

(define ext-env
  (λ (var val env)
    (λ (sym)
      (if (eqv? var sym)
          val
          (env sym)))))

(define empty-env
  (λ (sym)
    (error 'valof "unbound ~v" sym)))

(define env₁
  (λ (sym)
    (match sym
      [`A 5]
      [`B 10]
      [`C 15]
      [else (error 'valof "unbound ~v" sym)])))

#|equivalent to env₁|#
(define env₂
  (ext-env 'C 15
           (ext-env 'B 10
                    (ext-env 'A 5
                             empty-env))))

(define valof
  (λ (e env)
    (match e
      [`,y
       #:when (symbol? y)
       (env y)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (λ (a) (valof body (λ (y) (cond
                                   [(eqv? y x) a]
                                   [else (env y)]))))]
      [`,n
       #:when (number? n)
       n]
      [`(+ ,e₁ ,e₂)
       (+ (valof e₁ env) (valof e₂ env))]
      [`(* ,e₁ ,e₂)
       (* (valof e₁ env) (valof e₂ env))]
      [`(if ,test ,then ,alt)
       (if (valof test env)
           (valof then env)
           (valof alt env))]
      [`(zero? ,e₁)
       (zero? (valof e₁ env))]
      [`(,rator ,rand)
       ((valof rator env) (valof rand env))])))

