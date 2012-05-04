#lang racket
(provide make-mutable-set
         set-add! set-add*!
         set-remove! set-remove*!
         set-union!
         set-subtract!
         mutable-set-member?
         mutable-set->set)

(define (print-mutable-set s port mode)
  (parameterize ([current-output-port port])
    (if mode
        (write-string "(make-mutable-set")
        (write-string "#<mutable-set"))
    (if (unbox (mutable-set-s s))
        (for ((v (unbox (mutable-set-s s))))
          (write-string " ")
          (write v))
        (write-string "#f"))
    (if mode
        (write-string ")")
        (write-string ">"))
    (void)))

(struct mutable-set (s)
        #:property prop:custom-write print-mutable-set
        #:property prop:sequence (lambda (s) (in-set (unbox (mutable-set-s s))))
        #:transparent)

(define (make-mutable-set . args)
  (mutable-set (box (apply set args))))

(define (mutable-set->set s)
  (unbox (mutable-set-s s)))

(define (wrap f)
  (lambda (s . args)
    (set-box! (mutable-set-s s)
              (apply f (cons (unbox (mutable-set-s s)) args)))))
(define (wrap* f)
  (wrap (lambda (s . vs)
          (for/fold ((s s))
                    ((v vs))
            (f s v)))))

(define set-add! (wrap set-add))
(define set-add*! (wrap* set-add!))
(define set-remove! (wrap set-remove))
(define set-remove*! (wrap* set-remove!))
(define set-subtract! (wrap* set-subtract))
(define mutable-set-member? (wrap* set-member?))

(define (set-union! s1 . sets)
  (define (set-union2! s1 s2)
    (unless (mutable-set? s1) (error 'set-union! "expected mutable-set as first argument, given: ~a" s1))
    (cond [(mutable-set? s2) ((wrap set-union) s1 (unbox (mutable-set-s s2)))]
          [(set? s2) ((wrap set-union) s1 s2)]
          [else (error 'set-union! "cannot union non-set: ~a" s2)]))
  (for [(s2 sets)]
    (set-union2! s1 s2)))
