#lang racket

#|a store-passing style valof|#
#|
the type of env : variable -> box
the type of store : box -> value
the type of valof-sps : (exp, env, store) -> (store, value)
a value is often called a pure value
|#
(define valof-sps
  (λ (e env s)
    (match e
      [`,y
       #:when (symbol? y)
       `(,s ,(cdr (assv (env y) s)))]
      [`,n
       #:when (number? n)
       `(,s ,n)]
      [`(if ,test ,then ,alt)
       (match (valof-sps test env s)
         [`(,s^ ,b)
          (if b
              (valof-sps then env s^)
              (valof-sps alt env s^))])]
      [`(λ (,x) ,body)
       `(,s ,(λ (a s) (let ([new-box (length s)])
                        (valof-sps body
                                   (λ (y) (if (eqv? y x) new-box (env y)))
                                   `((,new-box . ,a) . ,s)))))]
      [`(set! ,x ,e) (match (valof-sps e env s)
                       [`(,s^ ,v)
                        (let ([xbox (env x)])
                          `(((,xbox . ,v) . ,s^) 'whatever))])]
      [`(begin ,e₁ ,e₂)
       (match (valof-sps e₁ env s)
         [`(,s^ ,_) (valof-sps e₂ env s^)])]
      [`(,rator ,rand)
       (match (valof-sps rator env s)
         [`(,s^ ,clos) (match (valof-sps rand env s^)
                         [`(,s^^ ,arg)
                          (clos arg s^^)])])])))

(valof-sps `((λ (x) (begin (set! x 10) x)) 5)
           (λ (y) (error "opps"))
           '())

#|Paul Hudak & Orbit & T|#
