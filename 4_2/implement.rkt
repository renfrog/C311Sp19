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
#|takes a goal and a stream, returns a stream|#
(define append-map$
  (λ (g $)
    (cond
      [(null? $) '()]
      [(pair? $)  (append$ (g (car $))
                           (append-map$ g (cdr $)))]
      [else (λ () (append-map$ g ($)))])))
#|takes two goals, returns one goal|#
(define disj₂
  (λ (g₁ g₂)
    (λ (s)
      (append$ (g₁ s) (g₂ s)))))
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
(define (reify v) 
  (λ (s)
    (let ([v (walk* v s)])
      (let ([substitution-of-names (reify-s v '())])
        (walk* v substitution-of-names)))))
(define (reify-name n)
  (string->symbol (string-append "_" (number->string n))))
(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s)
                       (walk* (cdr v) s))]
      [else v])))
(define (reify-s v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (let ([n (length s)])
                  (let ([rn (reify-name n)])
                    `((,v . ,rn) . ,s)))]
      [(pair? v) (let ([s (reify-s (car v) s)])
                   (reify-s (cdr v) s))]
      [else s])))

(define-syntax disj
  (syntax-rules ()
    [(disj) (fail)]
    [(disj g) g]
    [#|g ... means 0 or more gs|#
     (disj g₀ g ...)
     (disj₂ g₀ (disj g ...))]))
(define-syntax conj
  (syntax-rules ()
    [(conj) (succeed)]
    [(conj g) g]
    [#|g ... means 0 or more gs|#
     (conj g₀ g ...)
     (conj₂ g₀ (conj g ...))]))
(define-syntax defrel
  (syntax-rules ()
    [(defrel (name x ...) g ...)
     (define (name x ...)
       (λ (s)
         (λ ()
           ((conj g ...) s))))]))
(define-syntax run
  (syntax-rules ()
    [(run n (x₀ x ...) g ...)
     (run n q
       (fresh (x₀ x ...)
         (== `(,x₀ ,x ...) q) g ...))]
    [(run n q g ...)
     (let ([q (var 'q)])
       (map (reify q)
            (run-goal (conj g ...) n)))]))
(define-syntax run*
  (syntax-rules ()
    [(run* q g ...)
     (run #f q g ...)]))
(define-syntax fresh
  (syntax-rules ()
    [(fresh () g ...)
     (conj g ...)]
    [(fresh (x₀ x ...) g ...)
     (call/fresh
      (λ (x₀)
        (fresh (x ...) g ...))
      (var 'x₀))]))
(define-syntax condᵉ
  (syntax-rules ()
    [(condᵉ [g ...] ...)
     (disj (conj g ...) ...)]))
(define ((ifte g₁ g₂ g₃) S)
  (let loop ([$ (g₁ S)])
    (cond
      [(null? $) (g₃ S)]
      [(pair? $)
       (append-map$ g₂ $)]
      [else (λ () (loop ($)))])))
(define-syntax condᵃ
  (syntax-rules ()
    [(condᵃ (g₀ g ...)) (conj g₀ g ...)]
    [(condᵃ (g₀ g ...) ln ...)
     (ifte g₀ (conj g ...) (condᵃ ln ...))]))
(define ((once g) S)
  (let loop ([$ (g S)])
    (cond
      [(null? $) '()]
      [(pair? $) (cons (car $) '())]
      [else (λ () (loop ($)))])))
(define-syntax condᵘ
  (syntax-rules ()
    [(condᵘ (g₀ g ...) ...)
     (condᵃ ((once g₀) g ...) ...)]))

(defrel (fail)
  (== #f #t))
(defrel (succeed)
  (== #f #f))


(run 3 (q r)
  (condᵘ
   [(== q 2)]
   [(condᵉ [(== q 11)]
           [(== q 1)])
    (== r 'cat)]
   [(== q 3)]))
