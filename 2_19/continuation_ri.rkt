#lang racket

(require racket/trace)

(define apply-k
  (λ (k v)
    (match k
      [`(fib-sub2-k ,v^ ,k)
       (apply-k k (+ v^ v))]
      [`(fib-sub1-k ,n ,k)
       (fib-cps (sub1 (sub1 n))
                (make-fib-sub2-k v k))]
      [`(id-k) v]
      [`(rator-k ,rand ,env ,k)
       (valof-cps rand
                  env
                  (make-rand-k v k))]
      [`(rand-k ,c ,k)
       (c v k)]
      #;
      [whatever (k v)])))

(define make-fib-sub2-k
  (λ (v^ k) #|free variables: v k|#
    `(fib-sub2-k ,v^ ,k)
    #;
    (λ (w) (apply-k k (+ v^ w)))))

(define make-fib-sub1-k
  (λ (n k) #|free variables: n k|#
    `(fib-sub1-k ,n ,k)
    #;
    (λ (v)
      (fib-cps (sub1 (sub1 n))
               (make-fib-sub2-k v k)))))

(define make-id-k
  (λ () #|free variables: None|#
    `(id-k)
    #;
    (λ (v) v)))

(define fib-cps
  (λ (n k)
    (cond
      [(zero? n) (apply-k k 1)]
      [(zero? (sub1 n)) (apply-k k 1)]
      [else (fib-cps (sub1 n) (make-fib-sub1-k n k))])))

(trace fib-cps)

#;
(fib-cps 5 (make-id-k))

(define make-rator-k
  (λ (rand env k) #|fv: rand env k|#
    `(rator-k ,rand ,env ,k)
    #;
    (λ (c)
      (valof-cps rand
                 env
                 (make-rand-k c k)))))

(define make-rand-k
  (λ (c k) #|fv: c k|#
    `(rand-k ,c ,k)
    #;
    (λ (a)
      (c a k))))

(define valof-cps
  (λ (exp env k)
    (match exp
      [`,y
       #:when (symbol? y)
       (env y k)]
      [`,n
       #:when (number? n)
       (apply-k k n)]
      [`(λ (,x) ,body)
       (apply-k k (λ (a k)
                    (valof-cps body
                               (λ (y k) (if (eqv? y x)
                                            (apply-k k a)
                                            (env y k)))
                               k)))]
      [`(,rator ,rand)
       (valof-cps rator
                  env
                  (make-rator-k rand env k))])))

(valof-cps '(((λ (x) (λ (y) y)) 5) 6)
           (λ (y) (error "oopsie"))
           (make-id-k))
