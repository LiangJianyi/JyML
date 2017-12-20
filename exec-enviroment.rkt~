#lang racket
(require liangjianyi-racket/linkedlist)

(define (enviroment)
  (let ([eid -1]
        [object-tables null]
        [enviro-node (lambda (eid table)
                       (lambda (dispatch)
                         (cond [(eq? dispatch 'eid) eid]
                               [(eq? dispatch 'table) table])))])
    (lambda (opt)
      (cond [(eq? opt 'add)
             (set! eid (+ eid 1))
             (lambda (table)
               (set! object-tables [append-linkedlist object-tables (enviro-node eid table)]))]
            [(eq? opt 'table)
             (lambda (eid)
               (get-element-by-value object-tables eid
                                     (lambda (x)
                                       (when
                                           [equal? eid (x 'eid)]
                                         (x 'table)))))]
            [(eq? opt 'tail-eid) eid]))))

(define (object-table name value enviroment)
  (letrec ([row-index -1]
        [eid (((enviroment) 'tail-eid))]
        [address (lambda (dispatch)
                   (cond [(eq? dispatch 'eid) eid]
                         [(eq? dispatch 'row) row-index]))])
    (lambda (opt)
      (cond [(eq? opt 'name) name]
            [(eq? opt 'value) value]
            [(eq? opt 'address) address]))))