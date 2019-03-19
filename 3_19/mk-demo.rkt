#lang racket
(require "mk.rkt")
(define symbolᵒ symbolo)
(define numberᵒ numbero)
(define absentᵒ absento)

#;
(run 5 (q r s)
  (== `(cat ,r (a ,s (,r))) q)
  (conde
   [(== s 42)]
   [(== `(cat dog (a ,s (bird))) q)]
   [(== `(cat dog (a ,s (dog))) q)]
   ))

(define (append xs ys)
  (cond
    [(null? xs) ys]
    [else (match xs
            [`(,a . ,d)
             (let ([res (append d ys)])
               (cons a res))])]))

(defrel (appendᵒ xs ys o)
  (condᵉ
   [(== '() xs) (== ys o)]
   [(fresh (a d)
      (== `(,a . ,d) xs)
      (fresh (res)
        (== `(,a . ,res) o)
        (appendᵒ d ys res)))]))

(run 100 (xs ys zs)
  (appendᵒ xs ys zs))


