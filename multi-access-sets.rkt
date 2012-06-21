#lang racket

(provide (rename-out (make-multi-access-set multi-access-set))
         mas-primary-set
         mas-add
         mas-remove
         mas-contains-similar?
         mas-get-similar)

;; (define (empty-lens-hash lens)
;;   (make-custom-hash (lambda (x y)
;;                       (equal? (lens x) (lens y)))
;;                     (lambda (x)
;;                       (equal-hash-code (lens x)))))
;; (define-syntax lens-hash
;;   (syntax-rules ()
;;     ((lens-hash lens k v more ...)
;;      (dict-add (lens-hash lens more ...) k v))
;;     ((lens-hash lens)
;;      (empty-lens-hash lens))))
;; ;; A [PropertySet X] is a [LensHash X [SetOf X]]

;; A [PropertySet X] is a ∃Z.(property-set [Hash Z -> [SetOf X]] [X -> Z])
(struct property-set (map lens))
(define (empty-property-set lens)
  (property-set (hash) lens))

(define (ps-contains-similar? ps v)
  (match ps
    ((property-set map lens)
     (not (set-empty? (dict-ref map (lens v)))))))
(define (ps-get-similar ps v)
  (match ps
    ((property-set map lens)
     (dict-ref map (lens v) (set)))))
(define (ps-add ps v)
  (match ps
    ((property-set map lens)
     (property-set (dict-set map (lens v) (set-add (dict-ref map (lens v) (set)) v))
              lens))))
(define (ps-remove ps v)
  (match ps
    ((property-set map lens)
     (property-set (dict-set map (lens v) (set-remove (dict-ref map (lens v) (set)) v))
              lens))))
(define (ps-union a b)
  (match* (a b)
    (((property-set map1 lens1) (property-set map2 lens2))
     (unless (eq? lens1 lens2)
       (error 'ps-union
              "cannot union to property sets which don't have eq? lenses"))
     (let-values ([(map1 map2) (if (< (set-count map1 map2))
                                   (values map1 map2)
                                   (values map2 map1))])
       (for/fold ([map map2])
                 ([(k v) map1])
         (property-set (dict-set map k (set-union (dict-ref map k (set)) v))
                       lens1))))))

;; A [MultiAccessSet X] or [MAS X]
;;   (multi-access-set [SetOf X]
;;                     [Hash Symbol [PropertySet X]])
(struct multi-access-set (primary prop-sets)
        #:property prop:sequence (lambda (mas)
                                   (in-set (multi-access-set-primary mas))))
(define mas-primary-set multi-access-set-primary)
(define-syntax make-multi-access-set
  (syntax-rules ()
    ((_ (name lens) ...)
     (multi-access-set (set)
                       (for/hash ((n (list 'name ...))
                                  (l (list lens ...)))
                         (values n (empty-property-set l)))))))

(define (pointwise-lift mas set-f ps-f . args)
  (match mas
    ((multi-access-set primary prop-sets)
     (multi-access-set (set-f primary)
                       (for/hash ([(name prop-set) prop-sets])
                         (values name (ps-f prop-set)))))))
(define (mas-add mas v)
  (pointwise-lift mas
                  (lambda (s) (set-add s v))
                  (lambda (ps) (ps-add ps v))))
(define (mas-remove mas v)
  (pointwise-lift mas set-remove ps-remove v))
(define (mas-contains? mas v)
  (set-member? (multi-access-set-primary mas) v))
(define (property-lift mas property f)
  (f (dict-ref (multi-access-set-prop-sets mas) property)))
(define (mas-contains-similar? mas property v)
  (property-lift mas property
                 (lambda (ps)
                   (ps-contains-similar? ps v))))
(define (mas-get-similar mas property v)
  (property-lift mas property
                 (lambda (ps)
                   (ps-get-similar ps v))))
(define (mas-add-lens mas name lens)
  (match mas
    ((multi-access-set primary prop-sets)
     (when (dict-has-key? prop-sets name)
       (error 'mas-add-lens
              "We already track a property by the name ~a with lens ~a"
              name (property-set-lens (dict-ref prop-sets name))))
     (let* ([empty-prop-set (empty-property-set lens)]
            [new-prop-set (for/fold ([prop-set empty-prop-set])
                                    ([e primary])
                            (ps-add prop-set e))])
       (multi-access-set primary
                         (dict-set prop-sets
                                   name
                                   new-prop-set))))))
