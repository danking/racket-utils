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
         pset-get-one/rest
         pset->set
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
(define (pset->set ps)
  (for/fold
      ((s (set)))
      ((partition (in-dict-values (partitioned-set-ht ps))))
    (set-union s partition)))
(define (in-pset s)
  (in-set (pset->set s)))
(define (in-pset-partitions s)
  (in-dict-values (partitioned-set-ht s)))
;; Writer
(define (write-paritioned-set S port mode)
  (write-string "(partitioned-set " port)
  (for ((element (in-pset S)))
    (write element port)
    (write-string " " port))
  (write-string ")" port))
(define (partitioned-set-equal? S1 S2 recur)
  (and (for/and ((element (in-pset S1)))
         (pset-contains? S1 element))
       (for/and ((element (in-pset S2)))
         (pset-contains? S2 element))))
(define (partitioned-set-hash S recur)
  (recur (for/list ((element (in-pset S))) element)))
(define (partitioned-set-hash2 S recur)
  (recur (reverse (for/list ((element (in-pset S))) element))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The PartitionedSet structure
(struct partitioned-set (ht)
        #:transparent
        #:property prop:sequence in-pset
        #:methods gen:custom-write
        [(define write-proc write-paritioned-set)]
        #:methods gen:equal+hash
        [(define equal-proc partitioned-set-equal?)
         (define hash-proc partitioned-set-hash)
         (define hash2-proc partitioned-set-hash2)])
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
;; Returns a set containing all the elements in s save for the element v
(define (pset-remove s v)
  (let* ((ht (partitioned-set-ht s))
         (partition (dict-ref ht v (set))))
    (if (set-empty? partition)
        s
        (let ((partition-w/o-v (set-remove partition v)))
          (if (set-empty? partition-w/o-v)
              (partitioned-set (dict-remove ht v))
              (partitioned-set (dict-set ht v partition-w/o-v)))))))
(module+ test
  (check-true (= 0 (dict-count (partitioned-set-ht
                                (pset-remove (pset-add mt-pset 5) 5)))))
  (check-true (pset-empty? (pset-remove mt-pset 5)))
  (check-true (pset-empty? (pset-remove (pset-add mt-pset 5) 5)))
  (check-false (pset-empty? (pset-add (pset-remove mt-pset 5) 5)))
  (check-false (pset-empty? (pset-remove (pset-add mt-pset 5) 10)))
  (check-false (pset-empty? (pset-remove (pset-add mt-pset 5) 10)))
  (check-false (pset-empty? (pset-remove (pset-add (pset-add mt-pset 5) 6) 5))))

;; pset-get-one/rest : [PartitionedSet X] -> X [PartitionedSet X]
;;
;; Returns one element from the partitioned set S and a partitioned set
;; containing all the elements in S except for the aforementioned element.
;;
;; This is useful for workset algorithms.
;;
;; Throws an error if the set has no elements.
(define (pset-get-one/rest S)
  (let* ((ht (partitioned-set-ht S))
         (index (dict-iterate-first ht))
         (_ (unless index (error 'pset-get-one/rest
                                 "given partitioned set is empty")))
         (a-partition (dict-iterate-value ht index))
         (value (set-first a-partition)))
    (values value (pset-remove S value))))
(module+ test
  (let-values (((value S) (pset-get-one/rest (pset-add mt-pset 5))))
    (check-equal? value 5)
    (check-equal? S mt-pset))
  (let-values (((value S) (pset-get-one/rest (pset-add
                                              (pset-add
                                               mt-pset
                                               5)
                                              10))))
    (check-true (or (and (equal? value 5)
                         (equal? S (pset-add mt-pset 10)))
                    (and (equal? value 10)
                         (equal? S (pset-add mt-pset 5)))))
    (check-true (or (equal? S (pset-add mt-pset 5))
                    (equal? S (pset-add mt-pset 10)))))
  (let-values (((value S) (pset-get-one/rest (pset-add
                                              (pset-add
                                               mt-pset
                                               5)
                                              8))))
    (check-true (or (and (equal? value 5)
                         (equal? S (pset-add mt-pset 8)))
                    (and (equal? value 8)
                         (equal? S (pset-add mt-pset 5)))))))
