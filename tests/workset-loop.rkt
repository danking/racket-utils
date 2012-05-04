#lang racket
(require rackunit
         "../workset-loop.rkt")

(check-equal? (workset-loop (set 1) (hash) (e w h)
                            (for/fold ((w w) (preds h))
                                ((succ (hash-ref (hash 1 (set 2 3) 2 (set 4) 3 (set 4 5) 4 (set) 5 (set 1 2)) e)))
                              (let ((new-preds (hash-set preds succ
                                                         (set-add (hash-ref preds succ (set))
                                                                  e))))
                                (if (equal? new-preds preds)
                                    (values w preds)
                                    (values (set-add w succ)
                                            new-preds)))))
              (hash 1 (set 5) 2 (set 1 5) 3 (set 1) 4 (set 2 3) 5 (set 3)))