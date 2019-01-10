#lang racket

(define +
  (λ (n j)
    (cond
      [(zero? j) n]
      [else (add1 (+ n (sub1 j)))])))

(define *
  (λ (n j)
    (cond
      [(zero? j) 0]
      [else (+ n (* n (sub1 j)))])))

(define ↑
  (λ (n j)
    (cond
      [(zero? j) 1]
      [else (* n (↑ n (sub1 j)))])))

(define ⇑
  (λ (n j)
    (cond
      [(zero? j) 1]
      [else (↑ n (⇑ n (sub1 j)))])))

(define G
  (λ (i)
    (λ (n j)
      (cond
        [(zero? j)
         (cond
           [(zero? i) n]
           [(zero? (sub1 i)) 0]
           [else 1])]
        [(zero? i)
         (add1 ((G 0) n (sub1 j)))]
        [else
         ((G (sub1 i)) n ((G i) n (sub1 j)))]))))

(define A
  (λ (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (A (sub1 m) 1)]
      [else (A (sub1 m) (A m (sub1 n)))])))
