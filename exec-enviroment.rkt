#lang racket
(require liangjianyi-racket/linkedlist)
(require racket/exn)

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
               (set! object-tables [append-linkedlist object-tables [mcons (enviro-node eid table) null]]))]
            [(eq? opt 'table)
             (lambda (eid)
               (get-element-by-value object-tables eid
                                     (lambda (x)
                                       (when
                                           [equal? eid (x 'eid)]
                                         (x 'table)))))]
            [(eq? opt 'tables) object-tables]
            [(eq? opt 'tail-eid) eid]
            [else (error "error operator: " opt)]))))

(define (object-table enviroment)
  (letrec ([row-index -1]
           [eid (enviroment 'tail-eid)]
           [address (lambda (dispatch)
                      (cond [(eq? dispatch 'eid) eid]
                            [(eq? dispatch 'row) row-index]))]
           (add-entry (lambda (name value)
                        (set! objects [append-linkedlist objects [mcons (lambda (dispatch)
                                                                          (cond [(eq? dispatch 'name) name]
                                                                                [(eq? dispatch 'value) value]))
                                                                        null]])))
           (get-entry (lambda (name)
                        (letrec ([f (lambda (lik)
                                      [if [mpair? lik]
                                          (if [equal? name ([mcar lik] 'name)]
                                              [mcar lik]
                                              (f [mcdr lik]))
                                          (error "对象未定义: " name)])])
                          (f objects))))
           [objects null])
    (lambda (opt)
      (cond [(eq? opt 'address) address]
            [(eq? opt 'add)
             (set! row-index (+ row-index 1))
             ]
            [(eq? opt 'get) get-entry]))))