#lang racket

(define-syntax foo
  (syntax-rules ()
    [(foo x y) (cons x y)]
    [(foo x) (foo x x)]))
