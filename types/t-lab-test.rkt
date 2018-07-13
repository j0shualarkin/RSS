#lang s-exp "turnstile-lab.rkt"
; e.g., save this to file "typed-lang-prog.rkt"
(require turnstile/rackunit-typechecking)
; 
;(check-type 5 : Int)
;(check-type "five" : String)
; 
;(typecheck-fail #f #:with-msg "Unsupported literal")
;(typecheck-fail 1.1 #:with-msg "Unsupported literal")
; 
;(check-type + : (-> Int Int Int))
; 
;(check-type (+ 1 2) : Int -> 3)
; 
;(typecheck-fail (+ 1))
;
;;(check-type -2 : Nat)
;(check-type -2 : Int)

;(type-check-fail #f (+ 1))

(check-type (+ 1 1) : Int)

(check-type (if #f #t #f) : Boolean)
(check-type (if #f 2 3) : Int)
(typecheck-fail (if #f 2 "3")
                #:with-msg "expected Int, given String")

(check-type (λ ([x : String]) x) : (-> String String))



(check-type ((λ ([x : String]) x) "dogs") : String)
(typecheck-fail ((λ ([x : Int]) (+ x 7)) "dogs")
                #:with-msg "expected Int, given String")


(check-type (not #f) : Boolean)
(check-type (not #t) : Boolean)
(typecheck-fail (not 3)
                #:with-msg "expected Boolean, given Int")



(check-type (and #f #f) : Boolean)
(check-type (and #t #t) : Boolean)
(typecheck-fail (and 3 #f)
                #:with-msg "expected Boolean, given Int")
(typecheck-fail (and #t "3")
                #:with-msg "expected Boolean, given String")


(check-type (or #f #f) : Boolean)
(check-type (or #t #t) : Boolean)
(typecheck-fail (or 3 #f)
                #:with-msg "expected Boolean, given Int")
(typecheck-fail (or #t "3")
                #:with-msg "expected Boolean, given String")



;; write tests for numeric operators: *, -, zero?, sub1, add1













