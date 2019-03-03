#lang racket

(require racket/trace)

(define pc #f)
(define trampoline
  (λ (th)
    (trampoline (th))))

#|these globals variables are called registers|#
(define apply-k-k #f)
(define apply-k-v #f)
(define fib-cps-n #f)
(define fib-cps-k #f)

(define apply-k
  (λ () #|apply-k-k apply-k-v|#
    (match apply-k-k
      [`(fib-sub2-k ,v^ ,k)
       (begin [set! apply-k-k k]
              [set! apply-k-v (+ v^ apply-k-v)]
              (apply-k))]
      [`(fib-sub1-k ,n ,k)
       (begin [set! fib-cps-k (make-fib-sub2-k apply-k-v k)]
              [set! fib-cps-n (sub1 (sub1 n))]
              (fib-cps))]
      [`(id-k) apply-k-v])))

(define make-fib-sub2-k
  (λ (v^ k)
    `(fib-sub2-k ,v^ ,k)))

(define make-fib-sub1-k
  (λ (n k)
    `(fib-sub1-k ,n ,k)))

(define make-id-k
  (λ () 
    `(id-k)))

(define fib-cps
  (λ () #|fib-cps-n fib-cps-k|#
    (cond
      [(zero? fib-cps-n)
       (begin [set! apply-k-k fib-cps-k]
              [set! apply-k-v 1]
              (apply-k))]
      [(zero? (sub1 fib-cps-n))
       (begin [set! apply-k-k fib-cps-k]
              [set! apply-k-v 1]
              (apply-k))]
      [else
       (begin [set! fib-cps-k (make-fib-sub1-k fib-cps-n fib-cps-k)]
              [set! fib-cps-n (sub1 fib-cps-n)]
              (fib-cps))])))

(begin [set! fib-cps-k (make-id-k)]
       [set! fib-cps-n 5]
       (fib-cps))
