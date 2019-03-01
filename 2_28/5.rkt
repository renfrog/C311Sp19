#lang racket

#|1, define the program counter|#
#|2, turn every label invocation to set! pc|#
#|3, turn the test block to main|#
#|4, mount trampoline in main, and add jumpout to init-k|#
#|5, jumpout at apply-k|#
#|6, add print, run main|#

(require "parenthec.rkt")

(define-registers apply-k-k apply-k-v fib-cps-k fib-cps-n)
(define-program-counter pc)

(define-union continuation
  (init-k jumpout)
  (fib-sub1-k n k)
  (fib-sub2-k v^ k))

(define-label apply-k
  (union-case apply-k-k continuation
    [(fib-sub2-k v^ k)
     (begin (set! apply-k-k k)
            (set! apply-k-v (+ v^ apply-k-v))
            (set! pc apply-k))]
    [(fib-sub1-k n k)
     (begin (set! fib-cps-k (continuation_fib-sub2-k apply-k-v k))
            (set! fib-cps-n (sub1 (sub1 n)))
            (set! pc fib-cps))]
    [(init-k jumpout) (dismount-trampoline jumpout)]))

(define-label fib-cps
  (cond
    [(zero? fib-cps-n)
     (begin (set! apply-k-k fib-cps-k)
            (set! apply-k-v 1)
            (set! pc apply-k))]
    [(zero? (sub1 fib-cps-n))
     (begin (set! apply-k-k fib-cps-k)
            (set! apply-k-v 1)
            (set! pc apply-k))]
    [else
     (begin (set! fib-cps-k (continuation_fib-sub1-k fib-cps-n fib-cps-k))
            (set! fib-cps-n (sub1 fib-cps-n))
            (set! pc fib-cps))]))

(define-label main
  (begin (set! fib-cps-n 5)
         (set! pc fib-cps)
         (mount-trampoline continuation_init-k fib-cps-k pc)
         (printf "~s\n" apply-k-v)))

