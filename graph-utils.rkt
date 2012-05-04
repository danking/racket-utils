#lang racket
(require "set-utilities.rkt")
(require "workset-loop.rkt")
(provide (all-defined-out))

;; finds a representative member of every sink SCC and returns them all in a set
(define (find-sinks sources succs)
  (define (find-sinks workset seen sinks)
    (if (set-empty? workset)
        sinks
        (let-values (((e workset) (set-get-one/rest workset)))
          (let ((unseen-succs
                 (for/set ((s (succs e)) #:when (not (set-member? seen s))) s)))
            (if (set-empty? unseen-succs)
                (find-sinks workset (set-add seen e) (set-add sinks e))
                (find-sinks (set-union workset unseen-succs) (set-add seen e) sinks))))))
  (find-sinks sources (set) (set)))

(define (graph-fixed-point! initial-set succ-proc new-info? update)
  (workset-loop! initial-set (current workset)
    (for/fold ((workset workset))
        ((succ (succ-proc current)))
      (cond [(new-info? succ current)
             (update succ current)
             (set-add workset succ)]
            [else workset]))))

(define (graph-fixed-point initial-set initial-value succ-proc new-info? update)
  (workset-loop initial-set initial-value
                (current workset acc)
    (for/fold ((workset workset)
               (acc acc))
        ((succ (succ-proc current)))
      (cond [(new-info? acc succ current)
             (values (set-add workset succ)
                     (update acc succ current))]
            [else (values workset acc)]))))
