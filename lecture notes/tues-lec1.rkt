#lang racket/base

(require (only-in racket/list
                  [first fst]
                  [second snd])
         (rename-in racket/base
                    [or one-of]))

#;(module whatever racket
  (list 1 10)
  (define x 11)
  (module+ nested racket/base
    (require (submod ".."))
    (define my-secret-num 9)
    x
    (provide my-secret-number))
  (provide x)
  my-secret-number)

;; module* lets the submodule require the enclosing module
;; module+ you see all the surrounding stuff

(module+ main
  fst
  snd)

#;#;#;
(require (only-in racket/list
                  [first  fst]
                  [second snd])
         (rename-in racket/base
                    [or one-of])a)



(provide (rename-out [five my-fav-num]))
(define five 5)
