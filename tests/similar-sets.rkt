#lang racket
(require rackunit
         "../similar-sets.rkt")


;; set-contains
(check-true (let ((join (lambda (x y) x)))
              (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                (set join eq? eq? equal-hash-code 2 3 4))
                                     1)))
(check-true (let ((join (lambda (x y) x)))
              (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                (set join eq? eq? equal-hash-code 2 3 4))
                                     2)))
(check-true (let ((join (lambda (x y) x)))
              (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                (set join eq? eq? equal-hash-code 2 3 4))
                                     3)))
(check-true (let ((join (lambda (x y) x)))
              (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                (set join eq? eq? equal-hash-code 2 3 4))
                                     4)))
(check-false (let ((join (lambda (x y) x)))
               (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                 (set join eq? eq? equal-hash-code 2 3 4))
                                      5)))
(check-false (let ((join (lambda (x y) x)))
               (set-contains/similar? (set-union (set join eq? eq? equal-hash-code 1 2 3)
                                                 (set join eq? eq? equal-hash-code 2 3 4))
                                      0)))

;; set-contains -- more complex hash table procedures
(check-true (set-contains/similar?
             (let ((join (lambda (x y) (max x y)))
                   (similar? (lambda (x y) (= (floor (/ x 10)) (floor (/ y 10)))))
                   (hash-code (lambda (x) (equal-hash-code (floor (/ x 10))))))
               (set-union (set join eq? similar? hash-code 1 21 33)
                          (set join eq? similar? hash-code 25 32 47)))
             38))
(check-false (set-contains/equal?
              (let ((join (lambda (x y) (max x y)))
                    (similar? (lambda (x y) (= (floor (/ x 10)) (floor (/ y 10)))))
                    (hash-code (lambda (x) (equal-hash-code (floor (/ x 10))))))
                (set-union (set join eq? similar? hash-code 1 21 33)
                           (set join eq? similar? hash-code 25 32 47)))
              32))
(check-true (set-contains/equal?
             (let ((join (lambda (x y) (max x y)))
                   (similar? (lambda (x y) (= (floor (/ x 10)) (floor (/ y 10)))))
                   (hash-code (lambda (x) (equal-hash-code (floor (/ x 10))))))
               (set-union (set join eq? similar? hash-code 1 21 33)
                          (set join eq? similar? hash-code 25 32 47)))
             33))

;; set-get-similar
(check-eq? (set-get-similar
            (let ((join (lambda (x y) (max x y)))
                  (similar? (lambda (x y) (= (floor (/ x 10)) (floor (/ y 10)))))
                  (hash-code (lambda (x) (equal-hash-code (floor (/ x 10))))))
              (set-union (set join eq? similar? hash-code 1 21 33)
                         (set join eq? similar? hash-code 25 32 47)))
            30)
           33)
(check-eq? (set-get-similar
            (let ((join (lambda (x y) (max x y)))
                  (similar? (lambda (x y) (= (floor (/ x 10)) (floor (/ y 10)))))
                  (hash-code (lambda (x) (equal-hash-code (floor (/ x 10))))))
              (set-union (set join eq? similar? hash-code 1 21 33)
                         (set join eq? similar? hash-code 25 32 47)))
            32)
           33)