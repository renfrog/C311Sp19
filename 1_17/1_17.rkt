#lang racket

(define foo
  (λ (animal)
    (λ (color)
      `(Mary had a little ,animal
             whose fleece was ,color as
             ,(match color
                ['white 'snow]
                ['black 'dark-night])))))

#;
(equal? (cons 8 5)
        `(8 . 5))

(define rember8
  (λ (l)
    (match l
      ['() '()]
      #;
      [(cons 8 d) (rember8 d)]
      [`(8 . ,d) (rember8 d)]
      #;
      [(cons a d) (cons a (rember8 d))]
      [`(,a . ,d) `(,a . ,(rember8 d))])))

#;
(define ??
  (λ (e)
    (match e
      [`,y
       #:when (symbol? y)
       ]
      [`(λ (,x) ,body)
       #:when (symbol? x)
       ]
      [`(,rator ,rand)])))

#|0 in Church-encoding|#
(define zero
  (λ (zero)
    (λ (add1)
      zero)))

#|1 in Church-encoding|#
(define one
  (λ (zero)
    (λ (add1)
      (add1 zero))))

#|3 in Church-encoding|#
(define three
  (λ (zero)
    (λ (add1)
      (add1 (add1 (add1 zero))))))

#|+ in Church-encoding|#
(define plus
  (λ (cn₁) 
    (λ (cn₂)      
      (λ (zero) 
        (λ (add1) 
          ((cn₂ ((cn₁ zero) add1)) add1))))))

(define four
  ((plus one) three))

#;
((((plus four) four) 0) add1)
