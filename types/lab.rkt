#lang racket

; e.g., save this to file "morn-typed-lang.rkt"
(require (for-syntax syntax/parse syntax/stx
                     syntax/id-table
                     ))
(provide (rename-out [typechecking-mb #%module-begin])
         + if λ)

; A TyStx is a syntax object representing a type.
; A ExprStx is a syntax object representing an expression.
; A IdStx is an identifier syntax object.

(begin-for-syntax


; A TyEnv is a [ImmutableFreeIdTableOf IdStx -> TyStx]

; mk-empty-env : -> TyEnv
; Returns a new type environment with no bindings.
  (define (mk-empty-env)
    (make-immutable-free-id-table null))



  ; add-to-env : TyEnv IdStx TyStx -> TyEnv
  ; Returns a new type environment that extends the given env with the given binding.
  (define (extend-env Γ id τ)
    (free-id-table-set* Γ id τ))
  ; lookup-env : TyEnv IdStx -> TyStx or #f
  ; Looks up the given id in the given env and returns corresponding type. Returns false if the id is not in the env.
  (define (lookup-env Γ x)
    (free-id-table-ref Γ x #f))
  ; TODO: complete this function
  ; compute: ExprStx -> TyStx
  ; computes the type of the given term
  (define (compute e Γ)
    (syntax-parse e
      [(~literal +) #'(-> Int (-> Int Int))]
      [:id
           (let ([myb-type (lookup-env Γ e)])
             (if myb-type myb-type (raise-syntax-error 'compute/var "no type for given identifier:" e)))]
      [:integer #'Int]
      [:string #'String]
      [:boolean #'Bool]
      [((~literal if) e1 e2 e3)
       #:when (check #'e1 #'Bool Γ)
       (let ([τ (compute #'e2 Γ)])
         (if (check #'e3 τ Γ) τ
             (raise-syntax-error
              'compute/if
              (format "could not check type: ~a for ~a" (syntax->datum #'e3) (syntax->datum #'e2)))))]

      [((~literal +) e1 e2)
       (if (and (check #'e1 #'Int Γ) (check #'e2 #'Int Γ)) #'Int
           (raise-syntax-error 'compute/+
                               (format "error in types: ~a ~a" #'e1 #'e2)))]


      [((~literal λ) ([v:id : τ]) b) #`(-> τ #,(compute #'b (extend-env Γ #'v #'τ)))]

      [(e1 e2) (with-syntax ([A (compute #'e2 Γ)])
                   (let ([arrow (compute #'e1 Γ)])
                   (syntax-parse arrow
                     ((-> X Y) (if (type=? #'A #'X) #'Y
                                   (raise-syntax-error 'compute/apply
                                                       (format "expected operand to have type ~a got ~a"
                                                               (syntax-e #'X)
                                                               (syntax-e #'A))
                                                       e))))))]

      [e (raise-syntax-error
          'compute
          (format "could not compute type for term: ~a" (syntax->datum #'e))
          )]))

  ; check : ExprStx TyStx -> Bool
  ; checks that the given term has the given type
  (define (check e t-expected Γ)
    (define t (compute e Γ))
    (or (type=? t t-expected)
        (raise-syntax-error
         'check
         (format "error while checking term ~a: expected ~a; got ~a"
                 (syntax->datum e)
                 (syntax->datum t-expected)
                 (syntax->datum t))
         e)))

  ; type=? : TyStx TyStx -> Bool
  ; type equality here is is stx equality
  (define (type=? t1 t2)
    (or (and (identifier? t1) (identifier? t2) (free-identifier=? t1 t2))
        (and (stx-pair? t1) (stx-pair? t2)
             (= (length (syntax->list t1))
                (length (syntax->list t2)))
             (andmap type=? (syntax->list t1) (syntax->list t2)))))

  (define (do-typchecking stx Γ)
    (unless (null? stx)
      (begin (define data (stx-car stx))
             (syntax-parse data #:datum-literals (defun)
                           [(defun (name [x:id : t] ...) tout b) (do-typchecking (stx-cdr stx)
                                                                          (extend-env Γ
                                                                                      #'name
                                                                                      #'(-> t ... tout)))]
                           [data (begin
                                   (printf "~a : ~a\n" (syntax->datum #'data)
                                           (syntax->datum (compute #'data Γ)))
                                   (do-typchecking (stx-cdr stx) Γ))])))))



(define-syntax typechecking-mb
  (syntax-parser
    [(_ e ...)
    ; prints out each term e and its type, it if has one;
     ; otherwise raises type error
    ; this language only checks types,
     ; it doesn't run anything
     ;; #:do[(stx-map
     ;;       (λ (e)
     ;;         (printf "~a : ~a\n"
     ;;                 (syntax->datum e)
     ;;                 (syntax->datum (compute e (mk-empty-env)))))
     ;;       #'(e ...))]
     #:do[(do-typchecking #'(e ...) (mk-empty-env))]
     #'(#%module-begin (void))]

   ))
