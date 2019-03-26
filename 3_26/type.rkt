#lang racket

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

Γ ⊢ f   : (→ X B)
Γ ⊢ arg : X
------------------------ 
Γ ⊢ (f arg) : B

|#

