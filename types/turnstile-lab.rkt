#lang turnstile/quicklang
; e.g., save this to file "typed-lang.rkt"
(provide Int String Boolean Nat -> 
         (rename-out [typed-app #%app]
                     ;[typed-app/check #%app]
                     [typed-+ +]
                     [typed-minus -]
                     [typed-* *]
                     [typed-add1 add1]
                     [typed-sub1 sub1]
                     [typed-zero? zero?]
                     [typed-not not]
                     [typed-and and]
                     [typed-or or]
                     [typed-lambda λ]
                     [typed-if if]
                     [typed-datum #%datum]))
 
(define-base-types Int String Boolean Nat)
(define-type-constructor -> #:arity > 0)

;  \gg         ≫
;  \Rightarrow ⇒
;  \Leftarrow  ⇐
;  \vdash      ⊢

;; ==========================
;; Functions and Application
;; ==========================

(define-typerule (typed-app f e ...) ≫
  [⊢ f ≫ f- ⇒ (~-> τin ... τout)]
  #:fail-unless (= (stx-length #'(τin ...)) (stx-length #'(e ...)))
  (format "wrong amount of arguments for function ~a in: ~a \n expected: ~a"
          (syntax->datum #'f-)
          (syntax->datum #'(f- e ...))
          (length (syntax->list #'(τin ...))))
  [⊢ e ≫ e- ⇐ τin] ...
  --------------------
  [⊢ (#%plain-app f- e- ...) ⇒ τout])


(define-typerule typed-app/check 
  [(_ f e ...) ⇐ τo ≫ 
  [⊢ e ≫ e- ⇒ τ] ...
  [⊢ f ≫ f- ⇐ (-> τ ... τo)]
  --------------------------
  [⊢ (#%plain-app f- e- ...)]])


(define-typerule (typed-lambda ([x : τ]) body) ≫
  [[x ≫ x- : τ] ⊢ body ≫ b- ⇒ τ0 ]
  ------
  [⊢ (λ ([x- : τ]) b-) ⇒ (-> τ τ0)])


;; ==================
;; Base Types
;; ==================

(define-typerule typed-datum
  [(_ . n:integer) ≫
   -------------
   [⊢ (#%datum . n) ⇒ Int]]
  
  [(_ . b:boolean) ≫
   -------------
   [⊢ (#%datum . b) ⇒ Boolean]]
  
  [(_ . n:nat) ≫
   -------------
   [⊢ (#%datum . n) ⇒ Nat]]
  
  [(_ . s:str) ≫
   -------------
   [⊢ (#%datum . s) ⇒ String]]
  
  [(_ . x) ≫
   --------
   [#:error (type-error #:src #'x #:msg "Unsupported literal: ~v" #'x)]])



;; implememnt typing rules for
;; zero?, sub1, add1, *, fix, car, cdr, cons


;; ==================
;; Boolean Operators
;; ==================

(define-typerule (typed-not b) ≫
  [⊢ b ≫ b^ ⇐ Boolean]
  ----------
  [⊢ (not b^) ⇒ Boolean])

(define-typerule (typed-and p q) ≫
  [⊢ p ≫ p^ ⇐ Boolean]
  [⊢ q ≫ q^ ⇐ Boolean]
  ----------
  [⊢ (and p^ q^) ⇒ Boolean])

(define-typerule (typed-or p q) ≫
  [⊢ p ≫ p^ ⇐ Boolean]
  [⊢ q ≫ q^ ⇐ Boolean]
  ----------
  [⊢ (or p^ q^) ⇒ Boolean])

(define-typerule (typed-if Q C E) ≫
  [⊢ Q ≫ q ⇐ Boolean]
  [⊢ C ≫ c ⇒ τ]
  [⊢ E ≫ e ⇐ τ]
  ---------------
  [⊢ (if Q C E) ⇒ τ]
  )

;; ==================
;; Numeric Operators
;; ==================

(define-primop typed-+ +     : (-> Int Int Int))
(define-primop typed-* *     : (-> Int Int Int))
(define-primop typed-minus - : (-> Int Int Int))

(define-primop typed-sub1 sub1 : (-> Int Int))
(define-primop typed-add1 add1 : (-> Int Int))
(define-primop typed-zero? zero? : (-> Int Boolean))


;; ==================
;; String Operators
;; ==================

;; write a substring that can't be called unless the given indices are
;; within the length of the string

;; ==================
;; List Operators
;; ==================

;; translate the constructor for arrow types
;; to use cons for list types
;; (define-type-constructor -> #:arity > 0)





