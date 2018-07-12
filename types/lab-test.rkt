#lang s-exp "lab.rkt"
;And here is an example program to try with your typed language.
; e.g., save this to file "morn-typed-prog.rkt"
;5
;#f
;"five"
;; 1.1 ;; err
;; (if 1 2 3) ;; err
;(if #f 2 3)
;(if (if #f #t #f) 2 3)
;+
;(+ 1 2)
;;(+ #f 1) ;; err
;(λ ([x : String]) "dog")
;(+ 3)
;;(+ 3 "dog")
;((λ ([x : Int]) (+ 2 5)) 2)
;((λ ([x : Int]) x) 2)
;((λ ([x : String]) x) "dog")
;;((λ ([x : String]) x) 2) error
;(((λ ([x : Int])
;   (λ ([y : Int])
;     (+ x y))) 2) 3)
;#;
;(((λ ([x : Int])
;   (λ ([y : Int])
;     (+ x y))) 2) "dog") ;; err
#;
(λ ([x : String]) x)
#;
(((λ ([x : Int])
   (λ ([y : Int])
     (+ x y))) "cat") 3) ;; err


(defun (id [x Int]) Int x) ; (-> Int Int)
(id 5)



; Running the program prints:
; 5 : Int
; #f : Bool
; five : String
; (if #f 2 3) : Int
; + : (-> Int Int Int)
; (+ 1 2) : Int
