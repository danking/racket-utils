#lang racket
(provide set-get-one/rest)

(define (set-get-one/rest s)
  (let ((e (for/first ((e (in-set s))) e)))
    (values e (set-remove s e))))
