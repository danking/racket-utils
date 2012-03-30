#lang racket
(require "option.rkt")
(provide (rename-out (set/similar set))
         set-get-one/rest
         set-contains/similar?
         set-contains/equal?
         set-get-similar
         set-add
         set-remove/similar
         set-union
         set-empty?)

;; (make-set [Hash AState [Option Astate]])
;; The ht is a hash table whose equality predicate checks for state similarity,
;; i.e., that two states have the same node and stack. The values are either #f,
;; indicating the state is not in the set, or an astate indicating precisely
;; which astate is in the set


(define-struct set (ht join-proc equal?-proc similar?-proc hash-code-proc)
  #:transparent
  #:property prop:sequence (lambda (s)
                             (in-list (for/list ([(k maybe-v) (in-dict (set-ht s))])
                                        (match maybe-v
                                          ((some v) v)
                                          ((none) (error 'in-set/similar "sets should never contains (none)")))))))

(define (set-update-hash st ht)
  (match-let ([(set _ j e s hc) st])
    (make-set ht j e s hc)))
;; set-get-one/rest : [SimilarSet X] -> [Option [List X [SimilarSet x]]]
(define (set-get-one/rest s)
  (let* ((ht (set-ht s))
         (index (dict-iterate-first ht))
         (maybe-value (dict-iterate-value ht index)))
    (match maybe-value
      ((some value) (some (list value (set-remove/similar s value))))
      ((none)       (none)))))
(define (set-contains/similar? s v)
  (match (dict-ref (set-ht s) v (none))
    ((some _) #t)
    ((none)   #f)))

(define (set-contains/equal? s v)
  (let ((option (dict-ref (set-ht s) v (none)))
        (custom-equal? (set-equal?-proc s)))
    (match option
      ((some v2) (custom-equal? v v2))
      ((none) #f))))
(define (set-get-similar s v)
  (dict-ref (set-ht s) v (none)))
(define (set-add s v)
  (let ((ht (set-ht s))
        (custom-join (set-join-proc s)))
    (set-update-hash s
                     (hash-add/join ht v custom-join))))
(define (hash-add/join ht v custom-join)
  (match (dict-ref ht v (none))
    ((some v2) (dict-set ht v (some (custom-join v v2))))
    ((none)    (dict-set ht v (some v)))))
(define (set-remove/similar s v)
  (set-update-hash s (dict-remove (set-ht s) v)))
(define set-union
  ;; this was shamelessly stolen from collects/racket/set.rkt
  (case-lambda
    [(set) set]
    [(set1 set2)
     (match-let ([(set ht join equal? similar? hash-code) set1]
                 [(set ht2 join2 equal?2 similar?2 hash-code2) set2])
                (unless (eq? join join2)
                  (error 'set-union "sets' join procedures are not eq?"))
                (unless (eq? equal? equal?2)
                  (error 'set-union "sets' equal? predicates are not eq?"))
                (unless (eq? similar? similar?2)
                  (error 'set-union "sets' similar? predicates are not eq?"))
                (unless (eq? hash-code hash-code2)
                  (error 'set-union "sets' hash-code procedures are not eq?"))
                (let-values ([(ht ht2)
                              (if (> (dict-count ht2) (dict-count ht))
                                  (values ht2 ht)
                                  (values ht ht2))])
                  (make-set (for/fold ([ht ht]) ([v (in-dict-keys ht2)])
                              (hash-add/join ht v join))
                            join equal? similar? hash-code)))]
    [(set . sets)
     (for ([s (in-list (cons set sets))]
           [i (in-naturals)])
          (unless (set? s) (error 'set-union "not given a set ~a" s)))
     (for/fold ([set set]) ([set2 (in-list sets)])
       (set-union set set2))]))

;; needs a `custom-similar?' and `custom-hash' procedure
(define (empty-set/similar join equal? similar? hash-code)
  (make-set (make-immutable-custom-hash similar? hash-code)
            join
            equal?
            similar?
            hash-code))

(define (set-empty? s)
  (= 0 (dict-count (set-ht s))))

(define-syntax set/similar
  (syntax-rules ()
    ((_ join equal? similar? hash-code) (empty-set/similar join equal? similar? hash-code))
    ((_ join equal? similar? hash-code v others ...)
     (set-add (set/similar join equal? similar? hash-code others ...) v))))
