#lang racket

#|1, load the library|#
#|2, turn global variables to registers|#
#|3, turn define lambda-nils to define labels|#

(require "parenthec.rkt")

(define-registers apply-k-k apply-k-v fib-cps-k fib-cps-n)

(define make-init-k
  (λ ()
    `(init-k)))

(define make-fib-sub2-k
  (λ (v^ k)
    `(fib-sub2-k ,v^ ,k)))

(define make-fib-sub1-k
  (λ (n k)
    `(fib-sub1-k ,n ,k)))

(define-label apply-k
  (match apply-k-k
    [`(fib-sub2-k ,v^ ,k)
     (begin (set! apply-k-k k)
            (set! apply-k-v (+ v^ apply-k-v))
            (apply-k))]
    [`(fib-sub1-k ,n ,k)
     (begin (set! fib-cps-k (make-fib-sub2-k apply-k-v k))
            (set! fib-cps-n (sub1 (sub1 n)))
            (fib-cps))]
    [`(init-k) apply-k-v]))

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
     (begin (set! fib-cps-k (make-fib-sub1-k fib-cps-n fib-cps-k))
            (set! fib-cps-n (sub1 fib-cps-n))
            (fib-cps))]))

(begin (set! fib-cps-k (make-init-k))
       (set! fib-cps-n 5)
       (fib-cps))
