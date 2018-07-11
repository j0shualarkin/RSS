#lang racket

#|   Joshua Larkin
     Racket Summer School
     Day 1
     Matthias

;; porcupine pub and grill, west off campus down the hill, left on 1300 E, 6pm
;; Ugly is a technical term that I have coined

|#

(require rackunit)

(check-equal?
 (match '(a 1 2 3)
   [`(a ,x ,y) (+ x y)] ;; why doesnt y match on the cddr of the match expr
   [`(a ,x ,y ,z) (+ (* x y) z)])
 5)


#;(match '(a 1 (2 3))
    [`(a ,x ,y) (+ x y)]
    [`(a ,x ,y ,z) (+ (* x y) z)])


;  +: contract violation
;  expected: number?
;  given: '(2 3)
;  argument position: 2nd
;  other arguments...:


(check-equal?
 (match '(a 1 (2 3))
   [`(a ,x ,y) (+ x (car y))]
   [`(a ,x ,y ,z) (+ (* x y) z)])
 3)


(define-for-syntax second cadr)

(define-syntax (f x)
  (define e (syntax->list x))
  (second e))

#;#;
(f 17)
(f 17 32 05)

;; ---------

(define-syntax defstx
  (syntax-rules ()
    ((_ (name args) body) (define-syntax (name args) body))))

#|

syntax
(define-hello world) means the identifier 'world' is bound to "good bye"
translate htis to code of the shape (define 2nd-of-given-syntax "good")

ryan culpepper wrote syntax/parse

#`(define #,(second (syntax->list stx)) "good bye")

|#
(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

(defstx (define-goodbye stx)
  (syntax-parse stx
    [(_ name:id ...) #'(define-values (name ...)
                         (values (begin 'name "good bye") ...))]))
 
(define-goodbye world good bye damn it)

(check-equal? world
              "good bye")
(check-equal? good
              "good bye")
(check-equal? bye
              "good bye")
(check-equal? damn
              "good bye")
(check-equal? it
              "good bye")

;; (some e0:expr e1:expr ... en:expr)
;; produces the list of all non-#false results of the given exprs
;; until it encounters #false the syntax indicates that there is
;; at least one expr thro there might be arbitrarily many
;; if the first expr yield #f then (some ...) returns #f





(defstx (some stx)
  (syntax-parse stx
    [(_ e:expr) (combine #'e #f)]
    [(_ e0:expr e1:expr ...) (combine #'e0 #'(some e1 ...))]))



(define-for-syntax (combine e0 some-e1)
  #`(let ([v #,e0])
      (if v
          (let ([w #,some-e1])
            (if (cons? w)
                (cons v w)
                (list v)))
          #f)))



(check-equal? (some #f) #f)
(check-equal? (some 3) (list 3))
(check-equal? (some 1 2 #f 3) (list 1 2))
(check-equal? (some (begin (displayln "hello world") 3)) (list 3))

(check-equal? (some #f #f) #f)
(check-equal? (some 1 2 #f (displayln 'hello?)) (list 1 2))
(check-equal? (some #f #f #f #f #f #f) #f)



