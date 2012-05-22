#lang racket
(provide set-get-one/rest
         get-set-constructor
         for/set-union
         for/seteq-union
         for/seteqv-union)

(define (get-set-constructor s)
  (cond [(set-eq? s) seteq]
        [(set-eqv? s) seteqv]
        [(set-equal? s) set]))

(define (set-get-one/rest s)
  (let* ((e (for/first ((e (in-set s))) e))
         (s* (set-remove s e)))
    (unless (equal? (set-count s*)
                    (sub1 (set-count s)))
      (printf "[error] old-set: ")
      (pretty-print s)
      (printf "[error] new-set: ")
      (pretty-print s*)
      (printf "[error] element: ")
      (pretty-print e)
      (error 'set-get-one/rest
             "Removing an element from the set didn't change the set's size!?"))
    (values e s*)))

(define-for-syntax (gen-for/set-union set-type)
  (lambda (stx)
   (syntax-case stx ()
     [(_ clauses defs+exprs ...)
      #`(for/fold/derived stx
                          ([s (#,set-type)])
                          clauses
                          (set-union s
                                     (begin defs+exprs ...)))])))
(define-syntax for/set-union (gen-for/set-union 'set))
(define-syntax for/seteq-union (gen-for/set-union 'seteq))
(define-syntax for/seteqv-union (gen-for/set-union 'seteqv))
