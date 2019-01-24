#lang racket

(define ext-env
  (λ (x a env)
    `((,x . ,a) . ,env)
    #;
    (λ (y) (if (eqv? y x) a (apply-env env y)))))

(define apply-env
  (λ (env y)
    (match env
      ['() (error 'var "unbound ~v" y)]
      [`((,x . ,a) . ,env)
       (if (eqv? y x) a (apply-env env y))])
    #;
    (env y)))

(define init-env
  (λ ()
    '()
    #;
    (λ (y) (error 'var "unbound ~v" y))))

(define make-closure
  (λ (x body env)
    (λ (a) (valof body (ext-env x a env)))))

(define apply-closure
  (λ (clos a)
    (clos a)))

(define valof
  (λ (e env)
    (displayln e)
    (displayln env)
    (displayln "")
    (match e
      [`,n
       #:when (number? n)
       n]
      [`,y
       #:when (symbol? y)
       (apply-env env y)]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (make-closure x body env)]
      [`(,rator ,rand)
       (apply-closure (valof rator env) (valof rand env))])))

(valof '(((λ (x) (λ (y) x)) 5) 6)
       (init-env))

