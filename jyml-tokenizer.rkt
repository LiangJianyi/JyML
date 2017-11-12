#lang racket
(require liangjianyi-racket/linkedlist)

;;; 需单个字符作为 token 的字符一律划入白名单
(define tokenregular-whitelist
  (vector #\( #\)))
(define (find-tokenregular-whitelist? target)
  (define (stop? arg)
    (if [= arg (vector-length tokenregular-whitelist)]
        #f
        (if [equal? target (vector-ref tokenregular-whitelist arg)]
            #t
            #f)))
  (do ([index 0 (+ index 1)])
    (stop? index)))

;;; 依据黑名单提供的字符作为 seperator
(define tokenregular-blacklist
  (vector #\space #\newline))
(define (find-tokenregular-blacklist? target)
  (do ([index 0 (+ index 1)])
    ((or [= index (vector-length tokenregular-blacklist)]
         [equal? target (vector-ref tokenregular-blacklist index)])
     [when [equal? target (vector-ref tokenregular-blacklist index)]
       #t])))

;(define (procedure-table)
;  ())

(define (tokenizer exp)
  (let ([token-sequence null]
        [word ""]
        [current-char null])
    (do ([index 0 (+ index 1)])
      ([= index (string-length exp)])
      (begin
        (set! current-char [string-ref exp index])
        (if (find-tokenregular-whitelist? current-char)
            (set! token-sequence [append-linkedlist token-sequence (mcons current-char null)])
            (if (find-tokenregular-blacklist? current-char)
                (set! word [string-append word (string current-char)])
                (set! word "")))))))
