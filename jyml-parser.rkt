#lang racket
;(require "./jyml-tokenizer.rkt")
;(require "./jyml-ast.rkt")
;(require liangjianyi-racket/linkedlist)
;(require liangjianyi-racket/binarytree)
;(require compatibility/mlist)

(provide parse)
(provide read-jyml-file)

(define (parse tokens)
  (define list-begin-marks (list #\( #\[))
  (define list-end-marks (list #\) #\]))
  (define (f)
    (if [null? tokens]
        null
        [cond [(list? (member [car tokens] list-begin-marks))
               (set! tokens [cdr tokens])
               (cons (f) (f))]
              [(list? (member [car tokens] list-end-marks))
               (set! tokens [cdr tokens])
               null]
              [else
               (cons [car tokens]
                     (begin
                       (set! tokens [cdr tokens])
                       (f)))]]))
  (f))

(define (read-jyml-file path)
  (letrec ([iport (open-input-file path)]
           [f (lambda (c lik)
                (if [eof-object? c]
                    (begin
                      (close-input-port iport)
                      lik)
                    (f (read-char iport) (string-append lik (string c)))))])
    (f (read-char iport) "")))

