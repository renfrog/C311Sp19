#lang racket

#;
(let/cc k 
  (+ (k (k k)) (k 2)))

(define valof-cps
  (λ (exp env k)
    (match exp
      [`,n
       #:when (number? n)
       (k n)]
      [`,y
       #:when (symbol? y)
       (k (env y))]
      [`(let/cc ,k-name ,body)
       (valof-cps body
                  (λ (y) (if (eqv? y k-name)
                             k
                             (env y)))
                  k)]
      [`(+ ,x ,y)
       (valof-cps x env
                  (λ (x^)
                    (valof-cps y env
                               (λ (y^)
                                 (k (+ x^ y^))))))]
      [`(λ (,x) ,body)
       (k `(clos ,x ,body ,env))]
      [`(,rator ,rand)
       (valof-cps rand env
                  (λ (a)
                    (valof-cps rator env
                               (λ (clos)
                                 (match clos
                                   [`(clos ,x ,body ,env)
                                    (valof-cps body
                                               (λ (y) (if (eqv? y x)
                                                          a
                                                          (env y)))
                                               k)]
                                   [f (f a)])))))])))

(valof-cps '(let/cc k (+ 1 (k 2)))
           (λ (y) (error 'valof "unbound ~v" y))
           (λ (v) v))
