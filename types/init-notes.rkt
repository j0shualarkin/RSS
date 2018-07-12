#lang racket

#|

types, type rules, implementing type rules and typed languages

prevent bugs (1), avoid malware (2), prove equivalence (3)

lightweight, syntactic analysis that approximates program behavior
   -- just looks at the code [syntactic]
   -- adds predictablitiy [approximates]

validate funciton arguments (1) :: TypedRacket
check memory safety (2)         :: Rust
verify program properties (3)   :: Coq

you dont need a type system in your pl to do type-driven programming

|#

#|

surface code        -- -->
reader/lexer/parser -- [AST, syntax objects] -->
type checker        -- [checked ast] -->
compiler+           -- -->
core{byte|machine} code .


+(includes macro expander)

=================================================

1. incorporate types into grammar
2. create language of types
3. develop type rules for each language construct
4. implement a type checker



|#



#|

Definition = (defun (Identifier [Variable : Type] ...) : Type Expression)

Expr = (apply Expr Expr ...)
     | (λ ([x : Type] ...) Expr)
     | (if Expr Expr Expr)
     | (+ Expr Expr)
     | Number
     | String
     | Boolean
     | Variable

Type = (-> Type ...)
     | Number
     | Boolean
     | String

|#


#|

type rules

function-application ::


⊢ e1 : τ1 -> τ2  ⊢ e2 : τ1
___________________________
   ⊢ (apply e1 e2) : τ2




⊢ Q : Boolean, ⊢ C : τ1
___________________________
   ⊢ (if Q C A) : ?





|#











