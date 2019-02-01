#lang racket

(define init-env
  (λ ()
    (λ (oops) (error "unbound variable ~v" oops))))



(define val-of
  (λ (exp env)
    (match exp
      ((? symbol? y) (env y))
      ((? number? n) n)
      ((? boolean? b) b)
      (`(let ([,x ,e]) ,b) (val-of `((λ (,x) ,b) ,e) env))
      (`(λ (,x) ,b) (λ (a) (val-of b (λ (y) (if (eqv? y x) a (env y))))))
      (`(,rator ,rand) ((val-of rator env) (val-of rand env)))
      )))

(val-of '(let ([x 2])
  (let ([f (λ (e) x)])
    (let ([x 5])
      (f x))))
        (init-env))

(define val-of-dynamic
  (λ (exp env)
    (match exp
      ((? symbol? y) (env y))
      ((? number? n) n)
      ((? boolean? b) b)
      (`(let ([,x ,e]) ,b) (val-of-dynamic `((λ (,x) ,b) ,e) env))
      (`(* ,m ,n) (* (val-of-dynamic m env) (val-of-dynamic n env)))
      (`(λ (,x) ,b) `(clos ,x ,b))
      (`(,rator ,rand)
       (match (val-of-dynamic rator env)
         [`(clos ,x ,b)
          (val-of-dynamic b (λ (y) (if (eqv? y x) (val-of-dynamic rand env) (env y))))]))
      )))


(val-of-dynamic
 '(let ([x 2])
    (let ([f (λ (e) x)])
      (let ([x 5])
        (f x))))
        (init-env))

