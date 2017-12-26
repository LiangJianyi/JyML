#lang racket
(require liangjianyi-racket/linkedlist)
(require racket/exn)

;;; eid: 环境节点下标
;;; object-tables: 是个存储对象表的 mlist,每个节点是一个 enviro-node
;;; enviro-node: 环境节点，包含了eid和对象表
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
                                       (when [equal? eid (x 'eid)]
                                         (x 'table)))))]
            [(eq? opt 'tables) object-tables]
            [(eq? opt 'tail-eid) eid]
            [else (error "error operator: " opt)]))))

;;; 这里需要注意： address 是一个 proc，由 make-address 返回,
;;; 其带有一个参数和两个来自 make-address 的环境变量
;;; enviroment-id 在测试期间需要手动指定，当 object-table
;;; 真正添加到 enviroment 时，需要采取自动化措施填写该参数。
(define (object-table enviroment-id)
  ;;; letrec 绑定列表中的对象可以理解为一个类中的静态成员
  ;;; add-entry 接收的参数可以理解为与对象专属的实例成员
  (letrec ([row-index -1]
           [eid enviroment-id]
           (make-address (lambda (eid row)
                           (lambda (dispatch)
                             (cond [(eq? dispatch 'eid) eid]
                                   [(eq? dispatch 'row) row]))))
           (address-decode (lambda (code)
                             (if [string? code]
                                 (begin
                                   (if [> (length (string-split code "|")) 2]
                                       (error "地址编码非法: " code)
                                       (make-address (string->number (list-ref (string-split code "|") 0))
                                                     (string->number (list-ref (string-split code "|") 1)))))
                                 (error "code 必须是一个 string: " code))))
           (address-encode (lambda (addr)
                             (string-append
                              (string-append (number->string (addr 'eid)) "|")
                              (number->string (addr 'row)))))
           (add-entry (lambda (name value addr)
                        (set! objects [append-linkedlist objects [mcons (lambda (dispatch)
                                                                          (cond [(eq? dispatch 'addr) addr]
                                                                                [(eq? dispatch 'name) name]
                                                                                [(eq? dispatch 'value) value]))
                                                                        null]])))
           (eq-address? (lambda (addr1 addr2)
                          (and (equal? [addr1 'eid] [addr2 'eid])
                               (equal? [addr1 'row] [addr2 'row]))))
           (get-object-by-name (lambda (name)
                                 (letrec ([f (lambda (lik)
                                               [if [mpair? lik]
                                                   (if [equal? name ([mcar lik] 'name)]
                                                       [mcar lik]
                                                       (f [mcdr lik]))
                                                   (error "对象未定义: " name)])])
                                   (f objects))))
           (get-object-by-address (lambda (address)
                                    (let ([targets null])
                                      (iterator-linkedlist objects (lambda (x)
                                                                     (when [eq-address? (make-address [x 'current-eid] [x 'row]) address]
                                                                       (set! targets (append-linkedlist targets [mcons x null])))))
                                      [mcar targets])
                                    ))
           (display-object-info (lambda (obj)
                                  (display (address-encode [obj 'addr])) (display "  ")
                                  (display (obj 'name)) (display "  ")
                                  (display (obj 'value)) (newline)))
           ;;; 对象表
           [objects null])
    (lambda (opt)
      ;;; row-index 会随着对象在表中的分配与回收而改变其值
      ;;; current-row 操作能显示该全局状态目前滚动的位置（将该值加一可用作 object-count）
      (cond [(eq? opt 'current-eid) eid]
            [(eq? opt 'current-row) row-index]
            [(eq? opt 'add)
             (set! row-index (+ row-index 1))
             (lambda (name value)
               (with-handlers ([exn:fail? (lambda (e)
                                            (add-entry name
                                                       value
                                                       (make-address eid row-index)))])
                 (when [procedure? (get-object-by-name name)]
                   (error "对象重复定义: " name))))]
            [(eq? opt 'get-object-by-name) get-object-by-name]
            [(eq? opt 'get-object-by-address) get-object-by-address]
            [(eq? opt 'display-object-info) display-object-info]
            [else (error "无效的调用: " opt)]))))