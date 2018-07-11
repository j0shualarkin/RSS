#lang racket

(require (for-syntax syntax/parse))

(provide #%module-begin
         (rename-out [literal #%datum]
                     [plus +]
                     [bad-parens #%app])
         #%top-interaction)

(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . n:number)   #''n]
    [(_ . str:string) #''str]
    [(_ . b:boolean)  #''b]
    [(_ . oops)       (raise-syntax-error 'simple.rkt "not a literal!" #'oops)]))

(define-syntax (plus stx)
  (syntax-parse stx
    [(_ n1:expr n2:expr) (syntax/loc stx (+ n1 n2))] 
    [(_ . oops) (raise-syntax-error '+ "expects two arguments" #'oops)]))

(define-syntax (bad-parens stx)
  (raise-syntax-error 'application "not a procedure" stx))