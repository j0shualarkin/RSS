#lang racket

(require (for-syntax syntax/parse)
         rackunit)

(provide #%module-begin
         define-as-next
         #%top-interaction
         (rename-out [define-function define]
                     [literal #%datum]
                     [function-app #%app]
                     [ef if]))

;; =========================================================

(define-syntax (literal stx)
  (syntax-parse stx
    [(_ . n:number)   #''n]
    [(_ . str:string) #''str]
    [(_ . b:boolean)  #''b]
    [(_ . oops)       (raise-syntax-error 'simple.rkt "not a literal!" #'oops)]))

(define-syntax (ef stx)
  (syntax-parse stx #:literals (then else)
    [(_ Q:boolean then C:expr else E:expr) #'(if Q C E)]))

(check-equal? (ef #t then 2 else 4) 2)
(check-equal? (ef #f then 2 else 4) 4)

(define-syntax (then stx)
  (raise-syntax-error 'then "cannot use `then` outside of an `if` statement" stx))
(define-syntax (else stx)
  (raise-syntax-error 'else "cannot use `else` outside of an `if` statement" stx))


#|
Definition = (define-function (Variable Variable1 ...)  Expression)
 	 	 	 	 
Expression = (function-application Variable Expression ...)
          |  (if Expression Expression Expression)
          |  (+ Expression Expression)
          |  Variable
          |  Number
          |  String
          |  Boolean
|#

; ;; SYNTAX
; ;; (define-function (f x ...) e)
; ;; binds f to a syntax tranformer of shape (cons n s)
; ;; where n is the arity |x ...| of f
; ;; and   s is syntax for (λ (x ...) e)
 
(define-syntax (define-function stx)
  (syntax-parse stx
    [(_ (f:id parameter:id ...) body:expr)
     (define arity (length (syntax->list #'(parameter ...))))
     #`(define-syntax f (cons #,arity #'(lambda (parameter ...) body)))]))

; ;; SYNTAX
; ;; (function-app f e1 ... eN)
; ;; applies f to the values of e1 ... IF f is defined and f's arity is N 


;; Exercise 9 and 10

(define-syntax (function-app stx)
  (syntax-parse stx #:datum-literals (plus minus string+)
    [(_ plus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(+ arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ minus arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(- arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ string+ arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 2 n-args) #`(string-append arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    [(_ ++ arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (cond
       [(= 1 n-args) #`(add1 arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]
    
    [(_ f:id arg:expr ...)
     (define n-args (length (syntax->list #'(arg ...))))
     (define-values (arity the-function) (lookup #'f stx))
     (cond
       [(= arity n-args) #`(#,the-function arg ...)]
       [else
        (define msg (format "wrong number of arguments for ~a" (syntax-e #'f)))
        (raise-syntax-error #f msg stx)])]))


; Identifier Syntax -> (values N Id)
; EFFECT raises an exception if id is not available
(define-for-syntax (lookup id stx)
  ; -> Empty
  ; EFFECT abort process with syntax error 
  (define (failure)
    (define msg (format "undefined function: ~a" (syntax-e id)))
    (raise-syntax-error #f msg stx))
  (define result (syntax-local-value id failure))
  (values (car result) (cdr result)))

(check-equal? (function-app plus 1 2) 3)
(check-equal? (function-app plus (function-app plus 1 0) (function-app plus 0 2)) 3)

(check-equal? (function-app minus 1 2) -1)
(check-equal? (function-app minus (function-app minus 1 0) (function-app minus 2 0)) -1)
(check-equal? (function-app minus 9 (function-app minus 2 0)) 7)

(check-equal? (function-app string+ "9" (function-app string+ " 0" " 2")) "9 0 2")
(check-equal? (function-app string+ "cat" (function-app string+ " crow" (function-app string+ " dog" " cow")))
              "cat crow dog cow")

(check-equal? (function-app ++ 0) 1)
(check-equal? (function-app ++ (function-app ++ 2)) 4)
(check-equal? (function-app ++ (function-app plus (function-app ++ 3) (function-app minus 9 7))) 7)


;; =========================================================

;; EX 11
(define next 0)

(define-syntax (define-as-next stx)
  (syntax-parse stx
    ((_ var:id) #'(begin (define var next)
                         (set! next (add1 next))))))


(define-as-next x)
(define get-y
  (λ () (define-as-next y) y))
(define 1y (get-y))
(define another-y (get-y))

(check-equal? x 0)
(check-equal? 1y 1)
(check-equal? another-y 2)