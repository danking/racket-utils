#lang racket

(provide (all-defined-out))

(define empty-env (hash))
(define extend-env hash-set)
(define lookup-env hash-ref)
(define (extend-env/list e ks vs)
  (for/fold [(e e)]
            [(k ks)
             (v vs)]
    (extend-env e k v)))
