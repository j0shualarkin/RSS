#lang algebra

(define (next-sqr a) (plus (mult a a) (mult (-- a) (-- a))))

(app next-sqr 4)