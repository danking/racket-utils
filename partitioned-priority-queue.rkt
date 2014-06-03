#lang racket

(require (prefix-in splay: pfds/heap/splay)
         "partitioned-sets.rkt")

(provide ppq ppq-empty
         ppq-contains-equivclass?
         ppq-equivclass-partition
         ppq-partition-count
         ppq->set)

(struct ppq (priority-queue partitioned-set)
        #:methods gen:set
        [(define/match (set-member? _ x)
           [((ppq pq ps) _) (pset-contains? ps x)])
         (define/match (set-add _ x)
           [((ppq pq ps) _)
            (ppq (splay:insert x pq)
                 (pset-add ps x))])
         (define (set-add! ppq x)
           (error 'set-add! "unimplemented"))
         (define/match (set-remove _ x)
           [((ppq pq ps) _)
            (ppq (splay:remove (curry equal? x) pq)
                 (pset-remove ps x))])
         (define (set-remove! _ x)
           (error 'set-add! "unimplemented"))
         (define/match (set-first _)
           [((ppq pq ps))
            (splay:find-min/max pq)])
         (define/match (set-rest _x)
           [((ppq pq ps))
            (let ((x (splay:find-min/max pq)))
              (ppq (splay:delete-min/max pq)
                   (pset-remove ps x)))])
         (define/match (set-count _)
           [((ppq pq ps))
            (pset-count ps)
            ;; (assert-same "set-count: pq: ~a ps: ~a"
            ;;              (splay:fold (lambda (x y) (add1 x))
            ;;                          0
            ;;                          pq)
            ;;              (pset-count ps))
            ])
         (define/match (set-empty? _)
           [((ppq pq ps))
            (assert-same "set-empty?: pq: ~a pa: ~a"
                         (splay:empty? pq)
                         (pset-empty? ps))])])

(define (ps-lift f)
  (lambda (ppq . extra)
    (apply f (cons (ppq-partitioned-set ppq) extra))))

(define ppq-contains-equivclass? (ps-lift pset-contains-equivclass?))
(define ppq-equivclass-partition (ps-lift pset-equivclass-partition))
(define ppq-partition-count (ps-lift pset-partition-count))
(define ppq->set (ps-lift pset->set))

(define (ppq-empty <= same-partition? partition-hash-code)
  (ppq (splay:heap <=)
       (make-partitioned-set same-partition? partition-hash-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (assert-same failmsg a b)
  (if (equal? a b)
      a
      (error 'assertion-failure
             (format failmsg a b))))
