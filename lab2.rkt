#lang racket

(require (for-syntax syntax/parse)
         rackunit)


;; Exericse 6

;; sidenote: look up docs on syntax-parse to see how to
;; make bindings have distinct names for things that simulate let

(define-syntax (where stx)
  (syntax-parse stx
    ((_ e:expr [var:id e0:expr]) #'(let ([var e0])
                                     e))
    ((_ e:expr [var:id e0:expr] [v2:id e1:expr] ...) #'(let ([var e0])
                                                         (where e [v2 e1] ...)))))

(check-equal?
 (where (+ my-fav-num 2)
       [my-fav-num 8])
 10)

(check-equal?
 ((where (λ (x) (* x pi))
         [pi 3.14]) 1)
 3.14)

(check-equal? (where (op 10 (+ my-fav-num an-ok-num))
                     [my-fav-num 8]
                     [an-ok-num 2]
                     [op *])
              100)

(check-equal? (where (strapp good doggy)
                     [good "good "]
                     [doggy "doggy"]
                     [strapp string-append])
              "good doggy")


;; =========================================================

(define-syntax (where* stx)
  (syntax-parse stx
    [(_ body [var0:id expr0:expr] ...)
     #`(where body #,@(reverse (syntax->list #'([var0 expr0] ...))))]))

(check-equal? (where* (list x y z)
                      [x (+ y 4)]
                      [y (+ z 2)]
                      [z 1]) (list 7 3 1))

(check-equal? (where* (z x)
                      [x (+ y 2)]
                      [y 3]
                      [z ((λ (f)
                            (λ (n)
                              (if (zero? n) 1 (* n ((f f) (sub1 n))))))
                          (λ (f)
                            (λ (n)
                              (if (zero? n) 1 (* n ((f f) (sub1 n)))))))]) 120)



(check-equal? (where* (list x y z)
                      [x (+ y 4)]
                      [y (+ z 2)]
                      [z 1])
              (list 7 3 1))

(check-equal? (where* (string-append x (nts y) w)
                      [x "There are "]
                      [nts number->string]
                      [y (+ z 2)]
                      [w " dogs here"]
                      [z 2])
              "There are 4 dogs here")

;; for non-americans the SAT test is a dumb test that is below the drivers license

;; =========================================================

;; Exercise 7

(define-syntax (and/v stx)
  (syntax-parse stx #:literals (=>)
    ((_ e:expr => var:id e2:expr) #'(let ([var e])
                                      (if var e2 #f)))
    ((_ e:expr b:expr) #'(and e b))))

(define words-of-encouragment "YOU SUCK - Matthias and Robby")

(check-equal? (and/v 1 => x (+ x 1))
              2)

(check-equal? (and/v words-of-encouragment => str (substring str 11))
              "Matthias and Robby")

;; =========================================================

;; EX 8

(begin-for-syntax
  (define-syntax-class byte
    (pattern b:nat #:fail-unless (< (syntax-e #'b) 256) "not a byte")))
 
; SYNTAX
; (split-ct tags start end [name:id step (~optional convert)] ...)
; computes the values of the fields name... by successively extracting
; bytes from tags, beginning at start to maximally end
(define-syntax (split-ct stx)
  (syntax-parse stx
    [(_ tags start:integer end:byte [name step:byte (~optional convert)] ...)
     ; ———————————
     ; the static error checking 
     #:do [(define end-int  (syntax-e #'end))
           (define step-int (sum #'(step ...)))]
     #:fail-unless (< step-int end-int) "index out of range"
     ; ———————————
     #`(let ([i start])
         (let*-values ([(i name) (values (+ i step) (extract tags i (+ i step -1)))]
                       ...)
           (values ((~? convert values) name) ...)))]
    ;------------------------------------------------------------
    [(_ tags [name step:byte (~optional convert)] ...)
     ; ———————————
     ; the static error checking 
     #:do [(define end-int   (syntax-e #'256))
           (define step-int (sum #'(step ...)))]
     #:fail-unless (< step-int end-int) "index out of range"
     ; ———————————
     #`(let ([i 0])
         (let*-values ([(i name) (values (+ i step) (extract tags i (+ i step -1)))]
                       ...)
           (values ((~? convert values) name) ...)))]
    ))
 
; [Listof [Syntax Number]] -> Number
; compute the sum of the numbers hidden in syntax 
(define-for-syntax (sum list-of-syntax-numbers)
  (apply + (map syntax-e (syntax->list list-of-syntax-numbers))))


;; =========================================================

#|
Definition = (define-function (Variable Variable1 ...)  Expression)
 	 	 	 	 
Expression = (function-application Variable Expression ...)
          |  (if Expression Expression Expression)
          |  (+ Expression Expression)
          |  Variable
          |  Number
          |  String
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

#;(define-for-syntax next 0)

#;(define-syntax (define-as-next stx)
  (syntax-parse stx
    ((_ var:id) (set! next (+ next 1))
                #`(define var #,(sub1 next)))))

;(define-as-next x)
;(define-as-next y)
;(define-as-next z)

(define-as-next x)
(define get-y
  (λ () (define-as-next y) y))
(define 1y (get-y))
(define another-y (get-y))

(check-equal? x 0)
(check-equal? 1y 1)
(check-equal? another-y 2)




