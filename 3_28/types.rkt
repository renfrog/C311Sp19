#lang racket
(require "mk.rkt")

#|

n is a number
------------------------ 
Γ ⊢ n : Nat

Γ ⊢ n : Nat
Γ ⊢ m : Nat
------------------------ 
Γ ⊢ (+ n m) : Nat

Γ ⊢ test  : Bool 
Γ ⊢ then  : t
Γ ⊢ other : t
------------------------ 
Γ ⊢ (if test then other) : t

(lookup x Γ) is often written as Γ(x)

x is a var
(lookup x Γ) is t
------------------------ 
Γ ⊢ x : t

(ext x X Γ) is often written as Γ,(x:X)

(ext x X Γ) ⊢ body : B
------------------------ 
Γ ⊢ (λ (x) body) : (→ X B) 

Γ ⊢ rator : (→ X B)
Γ ⊢ rand  : X
------------------------ 
Γ ⊢ (rator rand) : B

|#

#|
maths Γ ⊢ e : τ
miniKanren (⊢ Γ e τ)
|#

(defrel (⊢ Γ e τ)
  (conde
   [(numbero e)
    (== 'Nat τ)]
   [(conde
     [(== #t e)]
     [(== #f e)])
    (== 'Bool τ)]
   [(fresh (n m)
      (== `(+ ,n ,m) e)
      (⊢ Γ n 'Nat)
      (⊢ Γ m 'Nat)
      (== 'Nat τ))]
   [(fresh (n)
      (== `(zero? ,n) e)
      (⊢ Γ n 'Nat)
      (== 'Bool τ))]
   [(fresh (n m)
      (== `(* ,n ,m) e)
      (⊢ Γ n 'Nat)
      (⊢ Γ m 'Nat)
      (== 'Nat τ))]
   [(fresh (n)
      (== `(sub1 ,n) e)
      (⊢ Γ n 'Nat)
      (== 'Nat τ))]
   [(fresh (test then other)
      (== `(if ,test ,then ,other) e)
      (⊢ Γ test 'Bool)
      (⊢ Γ then τ)
      (⊢ Γ other τ))]
   [(symbolo e)
    (lookupᵒ Γ e τ)]
   [(fresh (rand body τRand τBody)
      (== `(λ (,rand) ,body) e)
      (== `(→ ,τRand ,τBody) τ)
      (⊢ `((,rand . ,τRand) . ,Γ) body τBody))]
   [(fresh (rator rand τRand)
      (== `(,rator ,rand) e)
      (⊢ Γ rator `(→ ,τRand ,τ))
      (⊢ Γ rand τRand))]
   [#|
    Γ,f:τ ⊢ body : τ 
    ——————————————————
    Γ ⊢ (fix (λ (f) body)) : τ
    |#
    (fresh (f body)
      (== `(fix (λ (,f) ,body)) e)
      (⊢ `((,f . ,τ) . ,Γ) body τ))]))

(define (lookup Γ e)
  (match Γ
    ['() (error "oops")]
    [`((,x . ,ty) . ,Γ)
     (if (eqv? x e)
         ty
         (lookup Γ e))]))
(defrel (lookupᵒ Γ e τ)
  (fresh (x ty Γ^)
    (== `((,x . ,ty) . ,Γ^) Γ)
    (conde
     [(== x e) (== ty τ)]
     [(=/= x e) (lookupᵒ Γ^ e τ)])))

(define fact
  '(fix
    (λ (fact)
      (λ (n)
        (if (zero? n)
            1
            (* n (fact (sub1 n))))))))

(run 1000 e
  (⊢ '()
     `(fix
       (λ (fact)
         (λ (n)
           (* n ,e))))
     `(→ Nat Nat)))



