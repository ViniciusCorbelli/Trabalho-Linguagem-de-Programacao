; Aloizio Pita de Castro Júnior 201365006C
; Vinícius de Oliveira Corbelli 202065093C
#lang racket

(require dcc019/util/env
         dcc019/util/memory
         dcc019/exercise/iclasses/ast)

(provide value-of-program)

; value-of :: Exp -> ExpVal
(define (value-of exp Δ)
  (match exp
    [(ast:int v) v]
    [(ast:bool v) v]
    [(ast:dif e1 e2) (- (value-of e1 Δ) (value-of e2 Δ))]
    [(ast:zero? e) (zero? (value-of e Δ))]
    [(ast:not e) (value-of e Δ)]
    [(ast:if e1 e2 e3) (if (value-of e1 Δ) (value-of e2 Δ) (value-of e3 Δ))]
    [(ast:var v) (apply-env Δ v)] ; esta implementação só funciona para variáveis imutáveis
    [(ast:let (ast:var x) e1 e2) (value-of e2 (extend-env x (value-of e1 Δ) Δ))]
    [(ast:send e (ast:var mth) args) (display "send expression unimplemented")]
    [(ast:super (ast:var c) args) (display "super expression unimplemented")]
    [(ast:self) (display "self expression unimplemented")]
    [(ast:new (ast:var c) args) (display "new expression unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

; result-of :: Stmt -> Env -> State -> State
(define (result-of stmt Δ)
  (match stmt
    [(ast:assign (ast:var x) e)
     (let ([value (value-of e Δ)])
       (extend-env x value Δ))]
    [(ast:print e) (display (value-of e Δ))]
    [(ast:return e) (value-of e Δ)]
    [(ast:block stmts)
     (for-each (lambda (s) (result-of s Δ)) stmts)]
    [(ast:if-stmt e s1 s2)
     (if (value-of e Δ)
         (result-of s1 Δ)
         (result-of s2 Δ))]
    [(ast:while e s)
     (let loop ()
       (if (value-of e Δ)
           (begin
             (result-of s Δ)
             (loop))
           'done))]
    [(ast:local-decl (ast:var x) s)
     (let ([value (value-of x Δ)])
       (result-of s (extend-env x value Δ)))]
    [(ast:send e (ast:var mth) args) (display "command send unimplemented")]
    [(ast:super (ast:var c) args) (display "command super unimplemented")]
    [e (raise-user-error "unimplemented-construction: " e)]
    ))

(define (value-of-program prog)
  (empty-store)
  (match prog
    [(ast:prog decls stmt)
     (begin
       ; you must collect all the classes declared and building its respectively environment
       ; execute the prog expression in the correct environment
       (result-of stmt init-env))]))