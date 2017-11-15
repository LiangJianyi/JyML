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
            (set! token-sequence [append-linkedlist token-sequence (mcons current-char null)])
            (if (find-tokenregular-blacklist? current-char)
                (set! word "")
                (set! word [string-append word (string current-char)])))))))


;(define (tokenizer-2 exp)
;  (let ([token-sequence null]
;        [word ""]
;        [current-char null]
;        [whitelist? null]
;        [blacklist? null])
;    (do ([index 0 (+ index 1)])
;      ([= index (string-length exp)] token-sequence)
;      (begin
;        (set! current-char [string-ref exp index])
;        (set! whitelist? (find-tokenregular-whitelist? current-char))
;        (set! blacklist? (find-tokenregular-blacklist? current-char))
;        (if (whitelist?)
;            (set! token-sequence [append-linkedlist token-sequence (mcons current-char null)])
;            (if (blacklist?)
;                (set! word "")
;                (set! word [string-append word (string current-char)])))))))
