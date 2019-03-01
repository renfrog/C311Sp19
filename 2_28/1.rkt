#lang racket

(require "parenthec.rkt")

(define-registers apply-k-k apply-k-v fib-cps-n fib-cps-k)
(define-program-counter pc)

(define-union continuation
  (init-k jumpout)
  (fib-sub2-k v^ k)
  (fib-sub1-k n k))

(define-label apply-k #;(k v)
  (union-case apply-k-k continuation
              [(fib-sub2-k v^ k)
               (begin
                 (set! apply-k-k k)
                 (set! apply-k-v (+ v^ apply-k-v))
                 (set! pc apply-k))]
              [(fib-sub1-k n k)
               (begin
                 (set! fib-cps-k (continuation_fib-sub2-k apply-k-v k))
                 (set! fib-cps-n (sub1 (sub1 n)))
                 (set! pc fib-cps))]
              [(init-k jumpout) (dismount-trampoline jumpout)]))

(define-label fib-cps #;(n k)
  (cond
    [(zero? fib-cps-n)
     (begin
       (set! apply-k-k fib-cps-k)
       (set! apply-k-v 1)
       (set! pc apply-k))]
    [(zero? (sub1 fib-cps-n))
     (begin
       (set! apply-k-k fib-cps-k)
       (set! apply-k-v 1)
       (set! pc apply-k))]
    [else
     (begin
       (set! fib-cps-k (continuation_fib-sub1-k fib-cps-n fib-cps-k))
       (set! fib-cps-n (sub1 fib-cps-n))
       (set! pc fib-cps))]))

(define-label main
  (begin
    (set! fib-cps-n 5)
    (set! pc fib-cps)
    (mount-trampoline continuation_init-k fib-cps-k pc)
    (printf "~s\n" apply-k-v)))