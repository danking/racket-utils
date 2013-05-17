#lang racket

(provide make-partitioned-set
         pset-add
         pset-add*
         pset-contains?
         pset-contains-equivclass?
         pset-equivclass-partition
         pset-empty?
         pset-count
         pset-partition-count
         pset-remove
         in-pset
         in-pset-partitions)
(module+ test (require rackunit))

;; A [PartitionedSet X] is a (partitioned-set [Hash X [Set X]]
;;
;; The hash table's equivalence predicate and hash-code procedure define
;; equivalence classes on the domain X. Therefore, two elements which are
;; distinguishable by some other equivalence function (i.e. `equal?') will refer
;; to the same position in the hash table.
;;
;; The value associated with each position in the hash table is a set of
;; elements (which are indistinguishable to the hash table's equivalence
;; procedure) that form the intersection of some equivalence class with the set
;; represented by this PartitionedSet

;; Sequence Producers
(define (in-pset s)
  (in-set (for/fold
              ((s (set)))
              ((partition (in-dict-values (partitioned-set-ht s))))
            (set-union s partition))))
(define (in-pset-partitions s)
  (in-dict-values (partitioned-set-ht s)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The PartitionedSet structure
(struct partitioned-set (ht)
        #:transparent
        #:property prop:sequence in-pset)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (define mt-pset (make-partitioned-set (lambda (x y)
                                          (= (modulo x 5)
                                             (modulo y 5)))
                                        (lambda (x)
                                          (equal-hash-code (modulo x 5))))))

;; make-partitioned-set : [X X -> Boolean] [X -> Fixnum] -> [PartitionedSet X]
;;
(define (make-partitioned-set equivalence-predicate equivalence-hash-code)
  (partitioned-set (make-immutable-custom-hash equivalence-predicate
                                               equivalence-hash-code)))

;; pset-add : [PartitionedSet X] X -> [PartitionedSet X]
;;
;; Returns a PartitionedSet with contains all the elements in s as well as v.
(define (pset-add s v)
  (let* ((ht (partitioned-set-ht s))
         (partition (pset-equivclass-partition s v)))
    (partitioned-set (dict-set ht v (set-add partition v)))))

;; pset-add* : [PartitionedSet X] X ... -> [PartitionedSet X]
;;
;; Returns a new PartitionedSet which contains all the elements in s as well as
;; all subsequent arguments to pset-add*.
(define (pset-add* s . vs)
  (for/fold ([s s])
            ([v vs])
    (pset-add s v)))
(module+ test
  (check-equal? (pset-add* mt-pset)
                mt-pset)
  (check-equal? (pset-add* mt-pset 1)
                (pset-add mt-pset 1))
  (check-equal? (pset-add* mt-pset 1 2 3)
                (pset-add (pset-add (pset-add mt-pset 1) 2) 3))
  (check-equal? (pset-add* mt-pset 1 2 6)
                (pset-add (pset-add (pset-add mt-pset 1) 2) 6)))

;; pset-contains? : [PartitionedSet X] X -> Boolean
;;
;; Returns true iff v itself (as distinguished by equal?) is contained in the
;; PartitionedSet
(define (pset-contains? s v)
  (set-member? (pset-equivclass-partition s v) v))
(module+ test
  (check-true (pset-contains? (pset-add mt-pset 5) 5))
  (check-false (pset-contains? (pset-add mt-pset 5) 10))
  (check-false (pset-contains? (pset-add mt-pset 5) 0))
  (check-false (pset-contains? (pset-add mt-pset 5) 1))
  (check-false (pset-contains? (pset-add mt-pset 5) 2))
  (check-false (pset-contains? (pset-add mt-pset 5) 3))
  (check-false (pset-contains? (pset-add mt-pset 5) 4)))

;; pset-contains-equivclass? : [PartitionedSet X] X -> Boolean
;;
;; Returns true iff the intersection of PartitionedSet with the equivalence
;; class of v is non-empty.
(define (pset-contains-equivclass? s v)
  (not (set-empty? (pset-equivclass-partition s v))))
(module+ test
    (check-true (pset-contains-equivclass? (pset-add mt-pset 5) 0))
  (check-true (pset-contains-equivclass? (pset-add mt-pset 5) 5))
  (check-true (pset-contains-equivclass? (pset-add mt-pset 5) 10))
  (check-false (pset-contains-equivclass? (pset-add mt-pset 5) 1))
  (check-false (pset-contains-equivclass? (pset-add mt-pset 5) 2))
  (check-false (pset-contains-equivclass? (pset-add mt-pset 5) 3))
  (check-false (pset-contains-equivclass? (pset-add mt-pset 5) 4)))

;; pset-equivclass-partition : [PartitionedSet X] X -> [SetOf X]
;;
;; Returns the intersection of the PartitionedSet with the equivalence class of
;; v.
(define (pset-equivclass-partition s v)
  (dict-ref (partitioned-set-ht s) v (set)))
(module+ test
  (define example1
    (pset-add* mt-pset 0 5 30 3 8 1))

  (check-equal? (pset-equivclass-partition example1 11) (set 1))
  (check-equal? (pset-equivclass-partition example1 12) (set))
  (check-equal? (pset-equivclass-partition example1 13) (set 3 8))
  (check-equal? (pset-equivclass-partition example1 14) (set))
  (check-equal? (pset-equivclass-partition example1 15) (set 0 5 30)))

;; pset-empty? : [PartitionedSet X] -> Boolean
;;
;; Determines if PartitionedSet contains any elements. Runs in time linear on
;; the number of partitions.
(define (pset-empty? s)
  (for/and (((_ partition) (in-dict (partitioned-set-ht s))))
    (set-empty? partition)))
(module+ test
  (check-true (pset-empty? mt-pset))
  (check-false (pset-empty? (pset-add mt-pset 5))))

;; pset-count : [PartitionedSet X] -> Natural
;;
;; Returns the cardinality of the set. Runs in time linear on the number of
;; partitions.
(define (pset-count s)
  (for/sum (((_ partition) (in-dict (partitioned-set-ht s))))
    (set-count partition)))
(module+ test
  (check-equal? 0 (pset-count mt-pset))
  (check-equal? 1 (pset-count (pset-add mt-pset 1)))
  (check-equal? 2 (pset-count (pset-add (pset-add mt-pset 1) 2)))
  (check-equal? 1 (pset-count (pset-add (pset-add mt-pset 1) 1)))
  (check-equal? 2 (pset-count (pset-add (pset-add mt-pset 1) 6))))

;; pset-partition-count : [PartitionedSet X] -> Natural
;;
;; Returns the cardinality of the set of partitions in constant time.
(define (pset-partition-count s)
  (dict-count (partitioned-set-ht s)))
(module+ test
  (check-equal? 0 (pset-partition-count mt-pset))
  (check-equal? 1 (pset-partition-count (pset-add mt-pset 1)))
  (check-equal? 2 (pset-partition-count (pset-add (pset-add mt-pset 1) 2)))
  (check-equal? 1 (pset-partition-count (pset-add (pset-add mt-pset 1) 1)))
  (check-equal? 1 (pset-partition-count (pset-add (pset-add mt-pset 1) 6))))

;; pset-remove : [PartitionedSet X] X -> [PartitionedSet X]
;;
;; Returns a set containing all the elements in s save for the elements
(define (pset-remove s v)
  (let* ((ht (partitioned-set-ht s))
         (partition (dict-ref ht v (set))))
    (if (set-empty? partition)
        s
        (partitioned-set (dict-set ht v (set-remove partition v))))))
(module+ test
  (check-true (pset-empty? (pset-remove mt-pset 5)))
  (check-true (pset-empty? (pset-remove (pset-add mt-pset 5) 5)))
  (check-false (pset-empty? (pset-add (pset-remove mt-pset 5) 5)))
  (check-false (pset-empty? (pset-remove (pset-add mt-pset 5) 10)))
  (check-false (pset-empty? (pset-remove (pset-add mt-pset 5) 10)))
  (check-false (pset-empty? (pset-remove (pset-add (pset-add mt-pset 5) 6) 5))))
