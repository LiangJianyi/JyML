#lang racket
(require liangjianyi-racket/linkedlist)

;;; 需单个字符作为 token 的字符一律划入白名单
(define tokenregular-whitelist
  (vector #\( #\)))

(define (find-tokenregular-whitelist? target)
  (do ([index 0 (+ index 1)]
       [found? (equal? target (vector-ref tokenregular-whitelist 0))
               (equal? target (vector-ref tokenregular-whitelist index))])
    ((or [= index (vector-length tokenregular-whitelist)] found?)
     found?)))

;;; 依据黑名单提供的字符作为 seperator
(define tokenregular-blacklist
  (vector #\space #\newline))

(define (find-tokenregular-blacklist? target)
  (do ([index 0 (+ index 1)]
       [found? (equal? target (vector-ref tokenregular-blacklist 0))
               (equal? target (vector-ref tokenregular-blacklist index))])
    ((or [= index (vector-length tokenregular-blacklist)] found?)
     found?)))

;(define (procedure-table)
;  ())

(define (tokenizer exp)
  (let ([token-sequence null]
        [word ""]
        [current-char null])
    (do ([index 0 (+ index 1)])
      ([= index (string-length exp)] token-sequence)
      (begin
        (set! current-char [string-ref exp index])
        (if (find-tokenregular-whitelist? current-char)
            (if [equal? word ""]
                (set! token-sequence [append-linkedlist token-sequence (mcons current-char null)])
                (begin
                  (set! token-sequence [append-linkedlist token-sequence (mcons word null)])
                  (set! token-sequence [append-linkedlist token-sequence (mcons current-char null)])
                  (set! word "")))
            (if (find-tokenregular-blacklist? current-char)
                (unless [equal? word ""]
                  (set! token-sequence [append-linkedlist token-sequence (mcons word null)])
                  (set! word ""))
                (set! word [string-append word (string current-char)])))))))

