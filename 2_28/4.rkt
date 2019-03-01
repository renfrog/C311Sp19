#lang racket

#|1, turn the three continuation constructors to a union|#
#|2, to invoke a constructor c of union u, use u_c|#
#|3, to match the constructors, replace match with union-case|#

(require "parenthec.rkt")

(define-registers apply-k-k apply-k-v fib-cps-k fib-cps-n)

(define-union continuation
  (init-k)
  (fib-sub1-k n k)
  (fib-sub2-k v^ k))

(define-label apply-k
  (union-case apply-k-k continuation
    [(fib-sub2-k v^ k)
     (begin (set! apply-k-k k)
            (set! apply-k-v (+ v^ apply-k-v))
            (apply-k))]
    [(fib-sub1-k n k)
     (begin (set! fib-cps-k (continuation_fib-sub2-k apply-k-v k))
            (set! fib-cps-n (sub1 (sub1 n)))
            (fib-cps))]
    [(init-k) apply-k-v]))

(define-label fib-cps
  (cond
    [(zero? fib-cps-n)
     (begin (set! apply-k-k fib-cps-k)
            (set! apply-k-v 1)
            (apply-k))]
    [(zero? (sub1 fib-cps-n))
     (begin (set! apply-k-k fib-cps-k)
            (set! apply-k-v 1)
            (apply-k))]
    [else
     (begin (set! fib-cps-k (continuation_fib-sub1-k fib-cps-n fib-cps-k))
            (set! fib-cps-n (sub1 fib-cps-n))
            (fib-cps))]))

(begin (set! fib-cps-k (continuation_init-k))
       (set! fib-cps-n 5)
       (fib-cps))
