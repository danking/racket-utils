#lang racket
(require "set-utilities.rkt")
(provide (all-defined-out))

;;  - can I make current, workset, and value into match patterns?

(define-syntax workset-loop!
  (syntax-rules ()
    ((_ initial-set (current workset)
        body ...)
     (let loop ((workset initial-set))
       (unless (set-empty? workset)
         (let*-values (((current workset) (set-get-one/rest workset))
                       ((w) (let () body ...)))
           (loop w)))))))

(define-syntax workset-loop
  (syntax-rules ()
    ((_ initial-set initial-value (current workset value)
        body ...)
     (let loop ((workset initial-set)
                (value initial-value))
       (if (set-empty? workset)
           value
           (let*-values (((current workset) (set-get-one/rest workset))
                         ((w v) (let () body ...)))
             (loop w v)))))))




