#lang racket

(provide tri-partition-set)

(module+ test (require rackunit))

;; tri-partition-set : [X -> Boolean]
;;                     [X -> Boolean]
;;                     [SetOf X]
;;                     ->
;;                     [SetOf X]
;;                     [SetOf X]
;;                     [SetOf X]
;;
;; Partition the set such that all values for which the first-group? predicate
;; holds are in the first set, all the elements for which the second-group?
;; predicate holds are in the second set, and all other elements are in the
;; third set.
(define (tri-partition-set first-group? second-group? s)
  (for/fold
      ((first (set))
       (second (set))
       (third (set)))
      ((element (in-set s)))
    (cond [(first-group? element)
           (values (set-add first element) second third)]
          [(second-group? element)
           (values first (set-add second element) third)]
          [else
           (values first second (set-add third element))])))

(module+ test
  (let-values (((symbols strings others)
                (tri-partition-set symbol? string? '(a 1 "foo" 2 ds "bar" 3 4 "baz" 21))))
    (check-equal? symbols (set 'a 'ds))
    (check-equal? strings (set "foo" "bar" "baz"))
    (check-equal? others (set 1 2 3 4 21))))
