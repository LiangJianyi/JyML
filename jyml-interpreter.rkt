#lang racket
(require liangjianyi-racket/linkedlist)
(require liangjianyi-racket-sundry/utility)
(require "./jyml-parser.rkt")
(require "./exec-enviroment.rkt")
(require parser-tools/lex)

(provide eval)

(define (add node)
  (cond [(and [not (mpair? [mcar node])] [not (mpair? [mcdr node])])
         (+ [mcar node] [mcdr node])]
        [(and (mpair? [mcar node]) (mpair? [mcdr node]))
         (+ [calc (mcar node)] [calc (mcdr node)])]
        [(mpair? [mcar node])
         (+ [calc (mcar node)] [mcdr node])]
        [(mpair? [mcdr node])
         (+ [mcar node] [calc (mcdr node)])]))
(define (sub node)
  (cond [(and [not (mpair? [mcar node])] [not (mpair? [mcdr node])])
         (- [mcar node] [mcdr node])]
        [(and (mpair? [mcar node]) (mpair? [mcdr node]))
         (- [calc (mcar node)] [calc (mcdr node)])]
        [(mpair? [mcar node])
         (- [calc (mcar node)] [mcdr node])]
        [(mpair? [mcdr node])
         (- [mcar node] [calc (mcdr node)])]))
(define (multi node)
  (cond [(and [not (mpair? [mcar node])] [not (mpair? [mcdr node])])
         (* [mcar node] [mcdr node])]
        [(and (mpair? [mcar node]) (mpair? [mcdr node]))
         (* [calc (mcar node)] [calc (mcdr node)])]
        [(mpair? [mcar node])
         (* [calc (mcar node)] [mcdr node])]
        [(mpair? [mcdr node])
         (* [mcar node] [calc (mcdr node)])]))
(define (div node)
  (cond [(and [not (mpair? [mcar node])] [not (mpair? [mcdr node])])
         (/ [mcar node] [mcdr node])]
        [(and (mpair? [mcar node]) (mpair? [mcdr node]))
         (/ [calc (mcar node)] [calc (mcdr node)])]
        [(mpair? [mcar node])
         (/ [calc (mcar node)] [mcdr node])]
        [(mpair? [mcdr node])
         (/ [mcar node] [calc (mcdr node)])]))


;;; 判断一个数据是不是过程就看它是否在 procedures 表中存在
;;; 在表中登记的数据（关键字）视为 procedure，其类型为 symbol
;;; 当一个对象赋值为 lambda，那么该对象名就需要在表中登记
;;;
(define procedures null)
(set! procedures (append-linkedlist procedures [mcons (lambda (dispatch)
                                                        (cond [(eq? dispatch 'proc-name) '+]
                                                              [(eq? dispatch 'args-count) 'infinite]
                                                              [(eq? dispatch 'proc-body) add])) null]))
(set! procedures (append-linkedlist procedures [mcons (lambda (dispatch)
                                                        (cond [(eq? dispatch 'proc-name) '-]
                                                              [(eq? dispatch 'args-count) 'infinite]
                                                              [(eq? dispatch 'proc-body) sub])) null]))
(set! procedures (append-linkedlist procedures [mcons (lambda (dispatch)
                                                        (cond [(eq? dispatch 'proc-name) '*]
                                                              [(eq? dispatch 'args-count) 'infinite]
                                                              [(eq? dispatch 'proc-body) multi])) null]))
(set! procedures (append-linkedlist procedures [mcons (lambda (dispatch)
                                                        (cond [(eq? dispatch 'proc-name) '/]
                                                              [(eq? dispatch 'args-count) 'infinite]
                                                              [(eq? dispatch 'proc-body) div])) null]))

(define (get-proc-by-symbol lik key)
  (with-handlers ([exn:fail? (lambda (e) (error "未绑定的 procedure: " key))])
    [mcar (get-element-by-value lik key (lambda (x) (x 'proc-name)))]))

(define (calc node)
  (define left [mcar node])
  (define right [mcdr node])
  (cond [(symbol? left) ([get-proc-by-symbol procedures left] right)]
        [(number? left) left]
        [(mpair? left) (calc [mcar left]) (calc [mcdr left])]))


;;; name: 对象名字
;;; obj: 对象实体
;;; table: 环境对象表
(define (mydefine name obj table)
  ((table 'add) name obj))

(define (inferred-type node)
  (define number-collection (string->linkedlist "0123456789"))
  (define point #\.)
  (define minus-mark #\-)
  (define quote #\')
  (define double-quote #\")
  (define backslash #\\)
  (define boolean-collection (linkedlist "true" "false" "#t" "#f"))
  
  (define (build-number node)
    (define len (string-length node))
    ;;; 下面的 cond 完全可以通过 string->number 和异常处理来达到同样的目的，只不过我自己重新造了轮子罢了
    (cond [;;; 负号开头的分析过程
           (equal? (string-ref node 0) minus-mark)
           (cond [(= len 1) (error "类型无法识别: " node)]
                 [(or (= len 2) (= len 3)) (for ([e (substring node 1 len)])
                                             (when (or [not (find-node? number-collection e)]
                                                       [equal? point e])
                                               (error "类型无法识别: " node)))
                                           (string->number node)]
                 [(>= len 4) (if [or (equal? point (string-ref node 1)) (equal? point (string-ref node (- len 1)))]
                                 (error "类型无法识别: " node)
                                 (let ([point-count 0]
                                       [sub (substring node 1 len)])
                                   (for ([e sub])
                                     (if [> point-count 1]
                                         (error "类型无法识别: " node)
                                         (cond [(equal? point e) (set! point-count (+ point-count 1))]
                                               [(not (find-node? number-collection e)) (error "类型无法识别: " node)])))
                                   (string->number node)))])]
          [;;; 数字开头的分析过程
           (find-node? number-collection (string-ref node 0))
           (cond [(<= len 2)
                  (for ([e node])
                    (when [not (find-node? number-collection e)]
                      (error "类型无法识别: " node)))
                  (string->number node)]
                 [(>= len 3)
                  (let ([point-count 0]
                        [sub (substring node 1 (- len 1))])
                    (for ([e sub])
                      (if [> point-count 1]
                          (error "类型无法识别" node)
                          (cond [(equal? point e) (set! point-count (+ point-count 1))]
                                [(not (find-node? number-collection e)) (error "类型无法识别: " node)])))
                    (string->number node))])]))
  
  (define (lexica->boolean lex)
    (cond [(equal? lex "true") true]
          [(equal? lex "false") false]
          [(equal? lex "#t") #t]
          [(equal? lex "#f") #f]
          (error "类型无法识别: " lex)))

  (define (lexica->string lex)
    (substring lex 1 (- (string-length lex) 1)))
  
  [cond [(find-node? boolean-collection node) (lexica->boolean node)]
        [(and (equal? double-quote [string-ref node 0])
              (equal? double-quote [string-ref node (- [string-length node] 1)])) (lexica->string node)]
        [(find-node? procedures (string->symbol node)) (string->symbol "procedure")]
        [(equal? node "null") null]
        [else (cond [(equal? (string-ref node 0) quote) (string->symbol node)]
                    [(or (equal? (string-ref node 0) minus-mark)
                         (find-node? number-collection (string-ref node 0)))
                     (build-number node)]
                    [else (error "类型无法识别: " node)])]])


(define (eval ast)
  (cond [(or [number? ast]
             [symbol? ast]
             [char? ast]
             [string? ast]
             [boolean? ast]
             [null? ast])
         ast]
        [(pair? ast)
         (if [pair? ast]
             (begin
               (eval [car ast])
               (eval [cdr ast]))
             ((get-proc-by-symbol [string->symbol [car ast]]) [cdr ast]))]
        [else (error "I don't know!")]))