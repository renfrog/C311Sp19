#lang racket

(require racket/trace)

#|note: all (λ (v) (k v)) can be replaced with k|#

#|
1, if a simple expression is a result of another function,
then send it to k
2, if the simple expression is an input to another function,
then don't bother
(if the expression is a function then cps the function)
|#

(define valof-cps
  (λ (e env-cps k)
    (match e
      [`,y
       #:when (symbol? y)
       (env-cps y (λ (v) (k v)))]
      [`(let/cc ,k-name ,body)
       (valof-cps body
                     (λ (y env-k)
                       (cond
                         [(eqv? y k-name) (env-k k)]
                         [else (env-cps y (λ (v) (env-k v)))]))
                     (λ (v) (k v)))]
      [`(throw ,k-exp ,exp)
       (valof-cps k-exp env-cps
                  (λ (k)
                    (valof-cps exp env-cps k)))]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       (k
        (λ (a k)
          (valof-cps body
                     (λ (y k)
                       (cond
                         [(eqv? y x) (k a)]
                         [else (env-cps y (λ (v) (k v)))]))
                     (λ (v) (k v)))))]
      [`,n
       #:when (number? n)
       (k n)]
      [`(+ ,e₁ ,e₂)
       (valof-cps e₁ env-cps
                  (λ (n₁)
                    (valof-cps e₂ env-cps
                               (λ (n₂)
                                 (k (+ n₁ n₂))))))]
      [`(,rator ,rand)
       (valof-cps rator env-cps
                  (λ (clos)
                    (valof-cps rand env-cps
                               (λ (a)
                                 (clos a (λ (v) (k v)))))))])))

(trace valof-cps)

(define empty-env-cps
  (λ ()
    (λ (y k) (error 'value "oopsie"))))

(define init-k
  (λ ()
    (λ (v) v)))

#;
'(((λ (x) (λ (y) (+ x y))) 5) 6)

#;
(valof-cps '((λ (x) (x x)) (λ (x) (x x))) 
           (empty-env-cps)
           (init-k))

(define foo
  (λ (bar x)
    (λ (g)
      (cond
        [(bar x) (add1 x)]
        [else (g x)]))))

(define foo-cps
  (λ (bar-cps x k)
    (k (λ (g-cps k)
         (bar-cps x
                  (λ (b)
                    (cond
                      [b (k (add1 x))]
                      [else (g-cps x (λ (v) (k v)))])))))))

#;
(let/cc k (+ 5 6))
#|11|#
#;
(let/cc k (k (+ 5 6)))
#|11|#

#;
(let/cc k (k (+ (k 5) 6)))
#|5|#

#;
(let/cc k (k (+ (k 5) (k 6))))
#|still 5|#

(valof-cps '(let/cc k (throw k (+ (throw k 5) (throw k 6))))
           (empty-env-cps)
           (init-k))
