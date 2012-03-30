#lang racket
(provide some none)

;; An [Option X] is either
;;  - (some X), or
;;  - (none)
(define-struct some (v))
(define-struct none ())

