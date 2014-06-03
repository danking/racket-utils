#lang racket

(require data/heap
         "partitioned-sets.rkt")

(provide ppq ppq-empty
         ppq-contains-equivclass?
         ppq-equivclass-partition
         ppq-partition-count
         ppq->set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Partitioned Priority Queues
;;
;; NB: This implementation uses mutable heaps so you should use the data
;; structure linearly. Insertion and removal from the immutable heap accounted
;; for about 80% of analysis time :/
(struct ppq (priority-queue partitioned-set)
        #:methods gen:set
        [(define/match (set-member? _ x)
           [((ppq pq ps) _) (pset-contains? ps x)])
         (define/match (set-add _ x)
           [((ppq pq ps) _)
            (heap-add! pq x)
            (assert (string-append
                     (format "set-add: failed when adding ~a sizes were:" x)
                     "~a ~a")
                    (heap-count pq) (pset-count (pset-add ps x)))
            (ppq pq
                 (pset-add ps x))])
         (define (set-add! ppq x)
           (error 'set-add! "unimplemented"))
         (define/match (set-remove _ x)
           [((ppq pq ps) _)
            (heap-remove! pq x)
            (assert (string-append
                     (format "set-remove: failed when adding ~a sizes were:" x)
                     "~a ~a")
                    (heap-count pq) (pset-count (pset-remove ps x)))
            (ppq pq
                 (pset-remove ps x))])
         (define/match (set-subtract _ . others)
           [((ppq pq ps) (list other))
            (define new-ps
              (for/fold
                  ([ps ps])
                  ([x other])
                (heap-remove! pq x)
                (pset-remove ps x)))
            (ppq pq new-ps)]
           [((ppq pq ps) (list other other2 ...))
            (error 'variadic-set-subtract "unimplemented")])
         (define (set-remove! _ x)
           (error 'set-add! "unimplemented"))
         (define/match (set-first _)
           [((ppq pq ps))
            (heap-min pq)])
         (define/match (set-rest _x)
           [((ppq pq ps))
            (let ((x (heap-min pq)))
              (heap-remove-min! pq)
              (assert (string-append
                       (format "set-rest: failed when adding ~a sizes were:" x)
                       "~a ~a")
                      (heap-count pq) (pset-count (pset-remove ps x)))
              (ppq pq
                   (pset-remove ps x)))])
         (define/match (set-count _)
           [((ppq pq ps))
            (assert-same "set-count: pq: ~a ps: ~a"
                         (heap-count pq)
                         (pset-count ps))])
         (define/match (set-empty? _)
           [((ppq pq ps))
            (assert-same "set-empty?: pq: ~a ps: ~a"
                         (= (heap-count pq) 0)
                         (pset-empty? ps))])])

(define (ps-lift f)
  (lambda (ppq . extra)
    (apply f (cons (ppq-partitioned-set ppq) extra))))

(define ppq-contains-equivclass? (ps-lift pset-contains-equivclass?))
(define ppq-equivclass-partition (ps-lift pset-equivclass-partition))
(define ppq-partition-count (ps-lift pset-partition-count))
(define ppq->set (ps-lift pset->set))

(define (ppq-empty <= same-partition? partition-hash-code)
  (ppq (make-heap <=)
       (make-partitioned-set same-partition? partition-hash-code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(define (assert-same failmsg a b)
  (if (equal? a b)
      a
      (error 'assertion-failure
             (format failmsg a b))))

(define (assert failmsg a b)
  (unless (equal? a b)
    (error 'assertion-failure
           (format failmsg a b))))
