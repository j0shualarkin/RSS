#lang racket

;; ------------
;; Prelude
;; ------------

(require rackunit)
(require (for-syntax racket/list))
(require (for-syntax syntax/parse))


;; ===============
;; Exercise 5
;; ===============

(define-syntax (define-rewrite-rule stx)
  (syntax-parse stx #:datum-literals (-->)
    ((_ (name pattern ...) --> temp)
     #'(define-syntax (name stx)
         (syntax-parse stx
           [(_ pattern ...) #'temp])))))


;; ------------
;; Example 1
;; ------------
(define-rewrite-rule
  (ident x) --> x)

(check-equal? (ident 5) 5)
(check-equal? (ident 'a) 'a)
(check-equal? (ident (ident 6)) 6)


;; ------------
;; Example 2
;; ------------
(define-rewrite-rule (sum x ...) -->  (+ x ...))

(check-equal? (sum 5 5 5) (+ 5 5 5))
(check-equal? (sum 5 (sum 6 7 8) (sum 9 8 0)) (+ 5 6 7 8 9 8))


;; ===============
;; Exercise 1
;; ===============

(define-syntax (define-world-helper stx)
  (syntax-parse stx
    ((_ acc x:id) #'(define x acc))
    ((_ acc x:id y:id ...) #'(begin (define x acc)
                                    (define-world-helper (add1 acc) y ...)))))

(define-rewrite-rule (define-world* x ...) --> (define-world-helper 0 x ...))

;; -----------
;; Example 1
;; -----------
(define-world* a b c d)
(check-equal? a 0)
(check-equal? b 1)
(check-equal? c 2)
(check-equal? d 3)


;; ===============
;; Exercise 2 
;; ===============

(define-rewrite-rule (loop func ((arg val) ...) body ...) -->
  (let ()
    (define func (λ (arg ...) body ...))
    (func val ...)))


(check-equal? (loop sum ((ls (list 1 1 1))
                         (i 0))
                    (if (empty? ls) i (sum (cdr ls) (add1 i)))) 3)


(check-equal? (loop sum ((ls (list 1 1 1))
                         (i 0)
                         (j 0))
                    (if (empty? ls) (+ i j) (sum (cdr ls) (add1 i) (+ j i)))) 6)



;; ===============
;; Exercise 3
;; ===============

(define-syntax (all stx)
  (syntax-parse stx
    [(_ e0:expr) #`(let [(ne0 e0)]
                     (and ne0 `(,ne0)))]
    [(_ e0:expr e1:expr ...) #`(let [(ne0 e0)]                 
                                 (and ne0 (let [(rec (all e1 ...))]
                                            (and rec (cons ne0 rec)))))]))

(check-equal? (all #f) #f)

(check-equal? (all #t) '(#t))
(check-equal? (all #t #f) #f)
(check-equal? (all #f #t) #f)
(check-equal? (all #f #f) #f)
(check-equal? (all #t #t #t #t) '(#t #t #t #t))
(check-equal? (all #t #t #t #f) #f)
(check-equal? (all 1 2 3 #f) #f)
(check-equal? (all 1 2 3 4) '(1 2 3 4))


;; ===============
;; Exercise 4
;; ===============

(define-syntax (dispatch stx)
  (syntax-parse stx #:datum-literals (orelse)
    [(_ exp:expr [(x ...) expr0:expr])
     #'(if (member exp x ...) expr0 (error "oops"))]
    
    [(_ exp:expr [(x ...) expr0:expr] [orelse exprn:expr])
     #'(if (member exp x ...) expr0 exprn)]
    
    [(_ exp:expr [(x ...) expr0:expr] dc0 ...)
     #'(if (member exp x ...) (begin expr0 (dispatch exp dc0 ...)) (dispatch exp dc0 ...))]))


(define-syntax (member stx)
  (syntax-parse stx
    [(_ elem s1) #'(if (eqv? elem 's1) #t #f)]
    [(_ elem s1 s2 ...) #'(or (eqv? 's1 elem) (member elem s2 ...))]))

(define (f x)
  (dispatch x
            [(x y) (displayln 0)]
            [(z x) (displayln 1)]
            [orelse (displayln 2)]))


min