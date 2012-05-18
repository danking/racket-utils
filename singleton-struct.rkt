#lang racket
(require (for-syntax racket racket/syntax))
(provide (all-defined-out))

(define-syntax (singleton-struct stx)
  (syntax-case stx ()
    ((_ name others ...)
     (with-syntax ((name? (format-id #'name "~a?" (syntax-e #'name))))
       #'(define-values
           (name name?)
           (let ()
             (struct name () others ...)
             (values (name) name?)))))))
