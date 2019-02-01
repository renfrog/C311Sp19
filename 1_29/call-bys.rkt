#lang racket


(define (init-env)
  (λ (oops)
    (error 'empty-env "unbound variable ~v" oops)))
;
;;; call by value 
(define val-of/val
  (λ (exp env)
    (match exp
      (`,y #:when (symbol? y) (env y))
      (`,n #:when (number? n) n)
      (`(+ ,m ,n) (+ (val-of/val m env) (val-of/val n env)))
      (`(lambda (,x) ,body) (lambda (a)
                              (val-of/val body
                                          (lambda (y)
                                            (if (eqv? y x) a (env y))))))
      (`(,rator ,rand) ((val-of/val rator env) (val-of/val rand env))))))

;
;(val-of/val `((lambda (x) (+ x x)) 60) (init-env))
;(val-of/val `((lambda (x) (+ x x)) 120) (init-env))
;(val-of/val '(((lambda (x) (lambda (y) x)) 5) 6) (init-env)) ;; 5

;; `((lambda (x) (+ x x)) z) ;; z is bound `somewhere` to 15
; call by reference
(define val-of/ref
  (λ (exp env)
    (match exp
      (`,y #:when (symbol? y) (unbox (env y))) ;; look here
      (`,n #:when (number? n) n)
      (`(+ ,m ,n) (+ (val-of/ref m env) (val-of/ref n env)))
      (`(let ([,x ,e]) ,b) (val-of/ref `((lambda (,x) ,b) ,e) env))
      (`(lambda (,x) ,body) (lambda (a)
                              (val-of/ref body
                                          (lambda (y)
                                            (if (eqv? y x) a (env y))))))
      (`(,rator ,rand) ((val-of/ref rator env) (box (val-of/ref rand env)))))))

(val-of/ref `((lambda (x)
               (let ([a x])
                 (let ([b a])
                   (let ([c b])
                     (+ c c))))) 15) (init-env))


; (begin e0 e1 ...)
;; executes each expression (i.e. e0, e1 ...)
;; and returns the last expr given

;; box operators:
;; box : Value -> Reference
;; unbox : Reference -> Value
;; set-box! : Box -> Value -> Void

;; call by name
(define val-of/name
  (λ (exp env)
    (match exp
      (`,y #:when (symbol? y) (let ([y^ (env y)])
                                (begin (display (eq? y^ (env y)))
                                       ((unbox y^)))
                                )) 
      (`,n #:when (number? n) n)
      (`(print ,x) (display (val-of/name x env)))
      (`(+ ,m ,n) (+ (val-of/name m env) (val-of/name n env)))
      (`(* ,m ,n) (* (val-of/name m env) (val-of/name n env)))
      (`(let ([,x ,e]) ,b) (val-of/name `((lambda (,x) ,b) ,e) env))
      (`(if ,Q ,C ,E) (if (val-of/name Q env) (val-of/name C env) (val-of/name E env)))
      (`(zero? ,n) (zero? (val-of/name n env)))
      (`(sub1 ,n) (sub1 (val-of/name n env)))
      (`(lambda (,x) ,body) (lambda (a)
                              (val-of/name body
                                          (lambda (y)
                                            (if (eqv? y x) a (env y))))))
      (`(,rator ,rand) ((val-of/name rator env) (box (λ () (val-of/name rand env))))))))

#;(val-of/name '((lambda (x) (* (+ x x) x)) 5) (init-env))







(define ω (lambda (x) (x x)))
(define Ω (λ () (ω ω)))
(define ! (lambda (!)
            (lambda (n)
              (if (zero? n) 1 (* n ((! !) (sub1 n)))))))

#;(val-of/name (ω ω) (init-env))
#;(val-of/name ((lambda (x) x) (λ () Ω)) (init-env))
;(val-of/name ((lambda (x) 5) Ω) (init-env))
#;
(val-of/name `((lambda (x) (begin x
                                  (begin x
                                         x)))
               (((lambda (x) (x x))
                 (lambda (!)
                   (lambda (n)
                     (if (zero? n) 1 (* n ((! !) (sub1 n))))))) 5))
             (init-env))


#;(val-of/name ((ω !) 5) (init-env))


#;(val-of/name `((lambda (x)
               (let ([a x])
                 (let ([b a])
                   (let ([c b])
                     (+ c c))))) 15) (init-env))



;; call by need -- Haskell, "lazy eval"


(define val-of/need
  (λ (exp env)
    (match exp
      (`,y #:when (symbol? y) (let ([b (env y)])
                                (let ([th (unbox b)])
                                  (let ([ans (th)])
                                    (begin (set-box! b (λ () ans))
                                           ans))))) 
      (`,n #:when (number? n) n)
      (`(print ,x) (display (val-of/need x env)))
      (`(+ ,m ,n) (+ (val-of/need m env) (val-of/need n env)))
      (`(* ,m ,n) (* (val-of/need m env) (val-of/need n env)))
      (`(let ([,x ,e]) ,b) (val-of/need `((lambda (,x) ,b) ,e) env))
      (`(if ,Q ,C ,E) (if (val-of/need Q env) (val-of/need C env) (val-of/need E env)))
      (`(zero? ,n) (zero? (val-of/need n env)))
      (`(begin ,e1 ,e2) (begin (val-of/need e1 env)
                               (val-of/need e2 env)))
      (`(cons ,a ,d) `(,(λ () (val-of/need a env)) . ,(λ () (val-of/need d env))))
      (`(car `(,a . ,d)) (a))
      (`(cdr `(,a . ,d)) (d))
      (`(sub1 ,n) (sub1 (val-of/need n env)))
      (`(lambda (,x) ,body) (lambda (a)
                              (val-of/need body
                                          (lambda (y)
                                            (if (eqv? y x) a (env y))))))
      (`(,rator ,x) #:when (symbol? x) ((val-of/need rator env) (env x)))
      (`(,rator ,rand) ((val-of/need rator env) (box (λ () (val-of/need rand env))))))))

#;(val-of/need '(car (cons 5 (cons ((lambda (x) (x x))
                                  (lambda (x) (x x))) '()))) (init-env))

#;
(val-of/val '((lambda (x) (+ 6 6)) ((lambda (x) (x x))
                                    (lambda (x) (x x))))
            (init-env))

(val-of/name '((lambda (x) (+ 6 6)) ((lambda (x) (x x))
                               (lambda (x) (x x)))) (init-env))





