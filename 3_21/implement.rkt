#lang racket

(struct var
  (name)
  #:transparent)

#|
a value is one of the followings
1, a variable
2, a symbol/number/boolean/empty-list
3, a pair of two values
|#

#|checks whether x occurs in v|#
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? x v)]
      [(pair? v) (or (occurs? x (car v) s)
                     (occurs? x (cdr v) s))]
      [else #f])))
#|add a variable-value pair to a substitution|#
(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [else (cons `(,x . ,v) s)]))
#|
if v is a variable, then walk finds out
the meaning of v in s
|#
(define (walk v s)
  (cond
    [(var? v)
     (let ([pr (assv v s)])
       (if pr
           (walk (cdr pr) s)
           v))]
    [else v]))
#|
unify decides whether the two values are equal,
or what needs to be done to make them equal
|#
(define (unify v₁ v₂ s)
  (let ([v₁ (walk v₁ s)]
        [v₂ (walk v₂ s)])
    (cond
      [(eqv? v₁ v₂) s]
      [(var? v₁) (ext-s v₁ v₂ s)]
      [(var? v₂) (ext-s v₂ v₁ s)]
      [(and (pair? v₁) (pair? v₂))
       (let ([s (unify (car v₁) (car v₂) s)])
         (and s (unify (cdr v₁) (cdr v₂) s)))]
      [else #f])))

#;
(let ([x (var 'x)]
      [y (var 'y)]
      [z (var 'z)]
      [w (var 'w)])
  (let ([s '()])
    (unify x `(,x) s)))

#|== takes two values, returns a goal|#
#|
a goal is function that takes a substituion,
and returns a stream of substitutions
a goal fails by returning 0 substitution
|#
(define ==
  (λ (v₁ v₂)
    (λ (s)
      (let ([s (unify v₁ v₂ s)])
        (cond
          [s `(,s)]
          [else '()])))))

#|
a stream is one of the followings:
1, empty-list
2, consing a substitution on a stream
3, a thunk that returns a stream
|#
#|takes two streams, returns one stream|#
(define append$
  (λ ($₁ $₂)
    (cond
      [(null? $₁) $₂]
      [(pair? $₁) (cons (car $₁)
                        (append$ (cdr $₁) $₂))]
      [else (λ () (append$ $₂ ($₁)))])))
#|takes two goals, returns one goal|#
(define disj₂
  (λ (g₁ g₂)
    (λ (s)
      (append$ (g₁ s) (g₂ s)))))

#|takes a goal and a stream, returns a stream|#
(define append-map$
  (λ (g $)
    (cond
      [(null? $) '()]
      [(pair? $)  (append$ (g (car $))
                           (append-map$ g (cdr $)))]
      [else (λ () (append-map$ g ($)))])))
#|takes two goals, returns one goal|#
(define conj₂
  (λ (g₁ g₂)
    (λ (s)
      (append-map$ g₂ (g₁ s)))))

#|transforms an infinite stream to a finite list|#
(define take$
  (λ ($ n)
    (cond
      [(null? $) '()]
      [(zero? n) '()]
      [(pair? $) (cons (car $)
                       (take$ (cdr $) (sub1 n)))]
      [else (take$ ($) n)])))

#|
takes a function that returns a goal
and a name for a new variable,
returns a goal
|#
(define call/fresh
  (λ (f name)
    (f (var name))))

(define nats
  (λ (n)
    (cons n (λ () (nats (add1 n))))))
(define fibs
  (λ (n₁ n₂)
    (cons n₁ (λ () (fibs n₂ (+ n₁ n₂))))))

(define (run-goal g n)
  (take$ (g '()) n))

(run-goal (call/fresh
           (λ (x)
             (call/fresh
              (λ (y)
                (conj₂ (== x 1)
                       (== y 'cat)))
              'y))
           'x)
          10)
