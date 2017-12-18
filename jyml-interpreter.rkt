#lang racket
(require liangjianyi-racket/linkedlist)
(require liangjianyi-racket-sundry/utility)
(require "./jyml-parser.rkt")

(define (object-table opt enviroment)
  (define address ((enviroment 'eid)))
  (define entries null)
  (define (address-incre!)
    (let ([incre (substring (+ (string-first-index address #\|) 1)
                            (string-length address))]
          [area-code (substring address 0 (+ (string-first-index address #\|) 1))])
      (set! address (string-append area-code (+ (string->number incre) 1)))))
  (cond [(eq? opt 'add) (lambda (name obj)
                          (if [find-node? entries name (lambda (x) (list-ref x 1))]
                              (error "对象已经存在: " name)
                              (set! entries [append-linkedlist entries (mcons (list address name obj) null)])))]
        [(eq? opt 'get) (lambda (name)
                          (get-element-by-value entries name (lambda (x) (list-ref x 1))))]))

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
;;; name: 对象名字
;;; obj: 对象实体
;;; table: 环境对象表
(define (mydefine name obj table)
  ((table 'add) name obj))

(define procedures null)
(set! procedures (append-linkedlist procedures [mcons (mcons [quote +] add) null]))
(set! procedures (append-linkedlist procedures [mcons (mcons [quote -] sub) null]))
(set! procedures (append-linkedlist procedures [mcons (mcons [quote *] multi) null]))
(set! procedures (append-linkedlist procedures [mcons (mcons [quote /] div) null]))

(define (get-proc-by-symbol lik key)
  (unless (null? lik)
    (if (equal? key [mcar (mcar lik)])
        (mcdr [mcar lik])
        (get-proc-by-symbol [mcdr lik] key))))

(define (calc node)
  (define left [mcar node])
  (define right [mcdr node])
  (cond [(symbol? left) ([get-proc-by-symbol procedures left] right)]
        [(number? left) left]
        [(mpair? left) (calc [mcar left]) (calc [mcdr left])]))


(define test-exp-1
  (mcons '* (mcons (mcons '+ (mcons (mcons '/ (mcons 4 (mcons '- (mcons 100 98)))) 10)) 12)))
(define test-exp-2
  (mcons '+ (mcons (mcons '/ (mcons 100 10)) (mcons '* (mcons 2 5)))))
