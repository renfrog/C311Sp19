#lang racket
(require "monads.rkt")

(struct just
  (a)
  #:transparent)
(struct nothing
  ()
  #:transparent)

(define lookup
  (λ (l y)
    (match l
      ['() (nothing)]
      [`((,x . ,v) . ,l^)
       (if (eqv? y x)
           (just v)
           (lookup l^ y))])))

(define lookup-and-div2
  (λ (l y)
    (match (lookup l y)
      [(nothing) (nothing)]
      [(just a) (just (/ a 2))])))

#;
(define valof/counter
  (λ (exp env counter)
    (match exp
      [`,y
       #:when (symbol? y)
       `(,counter ,(env y))]
      [`,n
       #:when (number? n)
       `(,counter ,n)]
      [`(+ ,m ,n)
       (match (valof/counter m env (add1 counter))
         [`(,counter ,v₁)
          (match (valof/counter n env (add1 counter))
            [`(,counter ,v₂)
             `(,counter ,(+ v₁ v₂))])])]
      [`(λ (,x) ,body)
       `(,counter
         ,(λ (a counter) (valof/counter body
                           (λ (y) (if (eqv? y x) a (env y)))
                           (add1 counter))))]
      [`(,rator ,rand)
       (match (valof/counter rator env (add1 counter))
         [`(,counter ,clos)
          (match (valof/counter rand env (add1 counter))
            [`(,counter ,arg)
             (clos arg counter)])])])))

#;
(valof/counter `((λ (x) (+ 5 (+ 6 x))) 10)
           (λ (y) (error "oops"))
           0)

#|list -> list|#
(define rember8
  (λ (l)
    (cond
      [(null? l) '()]
      [(eqv? (car l) 8) (rember8 (cdr l))]
      [else (cons (car l) (rember8 (cdr l)))])))

#|list -> State list|#
(define rember8/counter
  (λ (l)
    (cond
      [(null? l) (inj-state '())]
      [(eqv? (car l) 8)
       (go-on ([result (rember8/counter (cdr l))]
               [counter (get)]
               [_ (put (add1 counter))])
         (inj-state result))
       #;
       (bind-state (rember8/counter (cdr l))
                   (λ (result)
                     (bind-state (get)
                                 (λ (counter)
                                   (bind-state (put (add1 counter))
                                               (λ (_)
                                                 (inj-state result)))))))]
      [else
       (go-on ([result (rember8/counter (cdr l))])
         (inj-state (cons (car l) result)))
       #;
       (bind-state (rember8/counter (cdr l))
                        (λ (result)
                          (inj-state (cons (car l) result))))])))

#;
((run-state
  (rember8/counter '(1 2 3 8 4 8)))
 0)

#|(state, a) -> (a , state)|#
(define swap/state
  (λ (ma)
    (bind-state ma
                (λ (a)
                  (bind-state (get)
                        (λ (s)
                          (bind-state (put a)
                                      (λ (_)
                                        (inj-state s)))))))))

#;
((run-state
  (swap/state (bind-state (put 'left)
                          (λ (_)
                            (inj-state 'right)))))
 0)

(define valof/counter
  (λ (exp env)
    (match exp
      [`,y
       #:when (symbol? y)
       (inj-state (env y))]
      [`,n
       #:when (number? n)
       (inj-state n)]
      [`(+ ,m ,n)
       (go-on ([counter (get)]
               [_ (put (add1 counter))]
               [v₁ (valof/counter m env)]
               [counter (get)]
               [_ (put (add1 counter))]
               [v₂ (valof/counter n env)])
         (inj-state (+ v₁ v₂)))]
      [`(λ (,x) ,body)
       (inj-state
        (λ (a) (go-on ([counter (get)]
                       [_ (put (add1 counter))])
                 (valof/counter body (λ (y) (if (eqv? y x) a (env y)))))))]
      [`(,rator ,rand)
       (go-on ([counter (get)]
               [_ (put (add1 counter))]
               [clos (valof/counter rator env)]
               [counter (get)]
               [_ (put (add1 counter))]
               [arg (valof/counter rand env)])
         (clos arg))])))

((run-state
   (valof/counter '(+ 5 (+ 6 8))
                  (λ (y) (error "oops"))))
 0)
